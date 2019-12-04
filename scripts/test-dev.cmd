:; set -e

:; # This will be the location of this script.
:; DIR="$(cd "$(dirname "$0")" && pwd)"

:; # Run XxxTests.exe with correct root set.
:; DS_ROOT="$DIR/../" esy x "TestDev.exe" "$@"
