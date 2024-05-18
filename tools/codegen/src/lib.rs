// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::{
    cmp::{self, Reverse},
    collections::BTreeMap,
    env, fmt,
    path::{Path, PathBuf},
    slice,
    str::FromStr,
    time::Duration,
};

use anyhow::{bail, Result};
use serde::{
    de::{self, Deserialize, Deserializer},
    ser::{Serialize, Serializer},
};
use serde_derive::{Deserialize, Serialize};

#[must_use]
pub fn workspace_root() -> PathBuf {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.pop(); // codegen
    dir.pop(); // tools
    dir
}

pub fn replace_vars(
    s: &str,
    package: &str,
    version: Option<&str>,
    platform: Option<HostPlatform>,
    rust_crate: Option<&str>,
) -> Result<String> {
    const RUST_SPECIFIC: &[(&str, fn(HostPlatform) -> &'static str)] = &[
        ("${rust_target}", HostPlatform::rust_target),
        ("${rust_target_arch}", HostPlatform::rust_target_arch),
        ("${rust_target_os}", HostPlatform::rust_target_os),
    ];
    let mut s = s.replace("${package}", package).replace("${tool}", package);
    if let Some(platform) = platform {
        s = s.replace("${exe}", platform.exe_suffix());
        if rust_crate.is_some() {
            for &(var, f) in RUST_SPECIFIC {
                s = s.replace(var, f(platform));
            }
        }
    }
    if let Some(version) = version {
        s = s.replace("${version}", version);
    }
    if s.contains('$') {
        for &(var, _) in RUST_SPECIFIC {
            if s.contains(var) {
                bail!(
                    "base manifest for {package} refers {var}, but 'rust_crate' field is not set"
                );
            }
        }
        bail!("variable not fully replaced: '{s}'");
    }
    Ok(s)
}

/// Download using GITHUB_TOKEN.
pub fn github_download(url: &str) -> Result<ureq::Response> {
    let mut token = env::var("GITHUB_TOKEN").ok().filter(|v| !v.is_empty());
    let mut retry = 0;
    let max_retry = 6;
    let mut last_error;
    loop {
        let mut req = ureq::get(url);
        if let Some(token) = &token {
            req = req.set("Authorization", token);
        }
        match req.call() {
            Ok(res) => return Ok(res),
            Err(e) => last_error = Some(e),
        }
        if retry == max_retry / 2 && token.is_some() {
            token = None;
        }
        retry += 1;
        if retry > max_retry {
            break;
        }
        eprintln!("download of {url} failed; retrying after {}s ({retry}/{max_retry})", retry * 2);
        std::thread::sleep(Duration::from_secs(retry * 2));
    }
    Err(last_error.unwrap().into())
}

/// Download without using GITHUB_TOKEN.
pub fn download(url: &str) -> Result<ureq::Response> {
    let mut retry = 0;
    let max_retry = 6;
    let mut last_error;
    loop {
        let req = ureq::get(url);
        match req.call() {
            Ok(res) => return Ok(res),
            Err(e) => last_error = Some(e),
        }
        retry += 1;
        if retry > max_retry {
            break;
        }
        eprintln!("download of {url} failed; retrying after {}s ({retry}/{max_retry})", retry * 2);
        std::thread::sleep(Duration::from_secs(retry * 2));
    }
    Err(last_error.unwrap().into())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: Option<u64>,
    pub minor: Option<u64>,
    pub patch: Option<u64>,
    pub pre: semver::Prerelease,
    pub build: semver::BuildMetadata,
}

impl Version {
    #[must_use]
    pub fn omitted(major: u64, minor: Option<u64>) -> Self {
        Self {
            major: Some(major),
            minor,
            patch: None,
            pre: semver::Prerelease::default(),
            build: semver::BuildMetadata::default(),
        }
    }
    #[must_use]
    pub fn latest() -> Self {
        Self {
            major: None,
            minor: None,
            patch: None,
            pre: semver::Prerelease::default(),
            build: semver::BuildMetadata::default(),
        }
    }
    #[must_use]
    pub fn to_semver(&self) -> Option<semver::Version> {
        Some(semver::Version {
            major: self.major?,
            minor: self.minor?,
            patch: self.patch?,
            pre: self.pre.clone(),
            build: self.build.clone(),
        })
    }
}
impl From<semver::Version> for Version {
    fn from(v: semver::Version) -> Self {
        Self {
            major: Some(v.major),
            minor: Some(v.minor),
            patch: Some(v.patch),
            pre: v.pre,
            build: v.build,
        }
    }
}
impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Version {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        pub(crate) fn convert(v: &Version) -> semver::Version {
            semver::Version {
                major: v.major.unwrap_or(u64::MAX),
                minor: v.minor.unwrap_or(u64::MAX),
                patch: v.patch.unwrap_or(u64::MAX),
                pre: v.pre.clone(),
                build: v.build.clone(),
            }
        }
        convert(self).cmp(&convert(other))
    }
}
impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(major) = self.major else {
            f.write_str("latest")?;
            return Ok(());
        };
        f.write_str(&major.to_string())?;
        let Some(minor) = self.minor else {
            return Ok(());
        };
        f.write_str(".")?;
        f.write_str(&minor.to_string())?;
        let Some(patch) = self.patch else {
            return Ok(());
        };
        f.write_str(".")?;
        f.write_str(&patch.to_string())?;
        if !self.pre.is_empty() {
            f.write_str("-")?;
            f.write_str(&self.pre)?;
        }
        if !self.build.is_empty() {
            f.write_str("+")?;
            f.write_str(&self.build)?;
        }
        Ok(())
    }
}
impl FromStr for Version {
    type Err = semver::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "latest" {
            return Ok(Self::latest());
        }
        match s.parse::<semver::Version>() {
            Ok(v) => Ok(v.into()),
            Err(e) => match s.parse::<semver::Comparator>() {
                Ok(v) => Ok(Self {
                    major: Some(v.major),
                    minor: v.minor,
                    patch: v.patch,
                    pre: semver::Prerelease::default(),
                    build: semver::BuildMetadata::default(),
                }),
                Err(_e) => Err(e),
            },
        }
    }
}
impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        String::serialize(&self.to_string(), serializer)
    }
}
impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer)?.parse().map_err(de::Error::custom)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Manifests {
    pub rust_crate: Option<String>,
    pub template: Option<ManifestTemplate>,
    #[serde(flatten)]
    pub map: BTreeMap<Reverse<Version>, ManifestRef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ManifestRef {
    Ref { version: Version },
    Real(Manifest),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    #[serde(flatten)]
    pub download_info: BTreeMap<HostPlatform, ManifestDownloadInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestDownloadInfo {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    pub checksum: String,
    /// Path to binaries in archive. Default to `${tool}${exe}`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bin: Option<StringOrArray>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestTemplate {
    #[serde(flatten)]
    pub download_info: BTreeMap<HostPlatform, ManifestTemplateDownloadInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestTemplateDownloadInfo {
    pub url: String,
    /// Path to binaries in archive. Default to `${tool}${exe}`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bin: Option<StringOrArray>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BaseManifest {
    /// Link to the GitHub repository.
    pub repository: String,
    /// Prefix of release tag.
    pub tag_prefix: String,
    /// Crate name, if this is Rust crate.
    pub rust_crate: Option<String>,
    pub default_major_version: Option<String>,
    /// Asset name patterns.
    pub asset_name: Option<StringOrArray>,
    /// Path to binaries in archive. Default to `${tool}${exe}`.
    pub bin: Option<StringOrArray>,
    pub signing: Option<Signing>,
    #[serde(default)]
    pub broken: Vec<semver::Version>,
    pub platform: BTreeMap<HostPlatform, BaseManifestPlatformInfo>,
    pub version_range: Option<String>,
}
impl BaseManifest {
    pub fn validate(&self) {
        for bin in self.bin.iter().chain(self.platform.values().flat_map(|m| &m.bin)) {
            assert!(!bin.as_slice().is_empty());
            for bin in bin.as_slice() {
                let file_name = Path::new(bin).file_name().unwrap().to_str().unwrap();
                if !self.repository.ends_with("/xbuild") {
                    assert!(
                        !(file_name.contains("${version") || file_name.contains("${rust")),
                        "{bin}"
                    );
                }
            }
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Signing {
    pub kind: SigningKind,
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
#[serde(deny_unknown_fields)]
pub enum SigningKind {
    /// algorithm: minisign
    /// public key: package.metadata.binstall.signing.pubkey at Cargo.toml
    /// <https://github.com/cargo-bins/cargo-binstall/blob/HEAD/SIGNING.md>
    MinisignBinstall,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BaseManifestPlatformInfo {
    /// Asset name patterns. Default to the value at `BaseManifest::asset_name`.
    pub asset_name: Option<StringOrArray>,
    /// Path to binaries in archive. Default to the value at `BaseManifest::bin`.
    pub bin: Option<StringOrArray>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum StringOrArray {
    String(String),
    Array(Vec<String>),
}

impl StringOrArray {
    #[must_use]
    pub fn as_slice(&self) -> &[String] {
        match self {
            Self::String(s) => slice::from_ref(s),
            Self::Array(v) => v,
        }
    }
    #[must_use]
    pub fn map(&self, mut f: impl FnMut(&String) -> String) -> Self {
        match self {
            Self::String(s) => Self::String(f(s)),
            Self::Array(v) => Self::Array(v.iter().map(f).collect()),
        }
    }
}

/// GitHub Actions Runner supports Linux (x86_64, aarch64, arm), Windows (x86_64, aarch64),
/// and macOS (x86_64, aarch64).
/// https://github.com/actions/runner/blob/v2.315.0/.github/workflows/build.yml#L21
/// https://docs.github.com/en/actions/hosting-your-own-runners/about-self-hosted-runners#supported-architectures-and-operating-systems-for-self-hosted-runners
///
/// Note:
/// - Static-linked binaries compiled for linux-musl will also work on linux-gnu systems and are
///   usually preferred over linux-gnu binaries because they can avoid glibc version issues.
///   (rustc enables statically linking for linux-musl by default, except for mips.)
/// - Binaries compiled for x86_64 macOS will usually also work on aarch64 macOS.
/// - Binaries compiled for x86_64 Windows will usually also work on aarch64 Windows 11+.
/// - Ignore arm for now, as we need to consider the version and whether hard-float is supported.
///   https://github.com/rust-lang/rustup/pull/593
///   https://github.com/cross-rs/cross/pull/1018
///   Does it seem only armv7l+ is supported?
///   https://github.com/actions/runner/blob/v2.315.0/src/Misc/externals.sh#L189
///   https://github.com/actions/runner/issues/688
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum HostPlatform {
    x86_64_linux_gnu,
    x86_64_linux_musl,
    x86_64_macos,
    x86_64_windows,
    aarch64_linux_gnu,
    aarch64_linux_musl,
    aarch64_macos,
    aarch64_windows,
}

impl HostPlatform {
    #[must_use]
    pub fn rust_target(self) -> &'static str {
        match self {
            Self::x86_64_linux_gnu => "x86_64-unknown-linux-gnu",
            Self::x86_64_linux_musl => "x86_64-unknown-linux-musl",
            Self::x86_64_macos => "x86_64-apple-darwin",
            Self::x86_64_windows => "x86_64-pc-windows-msvc",
            Self::aarch64_linux_gnu => "aarch64-unknown-linux-gnu",
            Self::aarch64_linux_musl => "aarch64-unknown-linux-musl",
            Self::aarch64_macos => "aarch64-apple-darwin",
            Self::aarch64_windows => "aarch64-pc-windows-msvc",
        }
    }
    #[must_use]
    pub fn rust_target_arch(self) -> &'static str {
        match self {
            Self::aarch64_linux_gnu
            | Self::aarch64_linux_musl
            | Self::aarch64_macos
            | Self::aarch64_windows => "aarch64",
            Self::x86_64_linux_gnu
            | Self::x86_64_linux_musl
            | Self::x86_64_macos
            | Self::x86_64_windows => "x86_64",
        }
    }
    #[must_use]
    pub fn rust_target_os(self) -> &'static str {
        match self {
            Self::aarch64_linux_gnu
            | Self::aarch64_linux_musl
            | Self::x86_64_linux_gnu
            | Self::x86_64_linux_musl => "linux",
            Self::aarch64_macos | Self::x86_64_macos => "macos",
            Self::aarch64_windows | Self::x86_64_windows => "windows",
        }
    }
    #[must_use]
    pub fn exe_suffix(self) -> &'static str {
        match self {
            Self::x86_64_windows | Self::aarch64_windows => ".exe",
            _ => "",
        }
    }
}

pub mod github {
    use serde_derive::Deserialize;

    // https://api.github.com/repos/<repo>/releases
    pub type Releases = Vec<Release>;

    // https://api.github.com/repos/<repo>/releases/<tag>
    #[derive(Debug, Deserialize)]
    pub struct Release {
        pub tag_name: String,
        pub prerelease: bool,
        pub assets: Vec<ReleaseAsset>,
    }

    #[derive(Debug, Deserialize)]
    pub struct ReleaseAsset {
        pub name: String,
        // pub content_type: String,
        pub browser_download_url: String,
    }
}

pub mod crates_io {
    use serde_derive::Deserialize;

    // https://crates.io/api/v1/crates/<crate>
    #[derive(Debug, Deserialize)]
    pub struct Crate {
        pub versions: Vec<Version>,
    }

    #[derive(Debug, Deserialize)]
    pub struct Version {
        pub checksum: String,
        pub dl_path: String,
        pub num: semver::Version,
        pub yanked: bool,
    }
}

pub mod cargo_manifest {
    use serde_derive::Deserialize;

    #[derive(Debug, Deserialize)]
    pub struct Manifest {
        pub package: Package,
    }

    #[derive(Debug, Deserialize)]
    pub struct Package {
        pub metadata: Metadata,
    }

    #[derive(Debug, Deserialize)]
    pub struct Metadata {
        pub binstall: Binstall,
    }

    #[derive(Debug, Deserialize)]
    pub struct Binstall {
        pub signing: BinstallSigning,
    }

    #[derive(Debug, Deserialize)]
    pub struct BinstallSigning {
        pub algorithm: String,
        pub pubkey: String,
    }
}
