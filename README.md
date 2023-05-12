### QBE IR Parser for Rust

qbe-reader is a Rust parser for v1.1 of the intermediate language of the [QBE compiler backend][qbe web].
The parser is presently in very early stages of development.
Most of the syntax is support but only a handful of instructions are properly parsed presently.
For this reason, the API is not stable and still subject to change.

## Usage

The library is presently not available on crates.io.
However, for now it is possible to depend directly on the Git repository:

    [dependencies]
    qbe_reader = { git = "https://git.8pit.net/qbe-reader.git" }

Keep in mind though, that the API provided by qbe-reader is not yet stable though.

## See also

* The [documentation of the QBL IL][qbe-il 1.1]
* [qbe-rs][qbe-rs github] an unparser for the QBE IL in Rust

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero
General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.

[qbe web]: https://c9x.me/compile/
[qbe-rs github]: https://github.com/garritfra/qbe-rs
[qbe-il 1.1]: https://c9x.me/compile/doc/il-v1.1.html
