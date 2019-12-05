:; set -e

:; # This will be the location of this script.
:; DIR="$(cd "$(dirname "$0")" && pwd)"

:; # Run RargTests.exe with correct root set.
:; RARG_ROOT="$DIR/../" esy x "TestDev.exe" "$@"
