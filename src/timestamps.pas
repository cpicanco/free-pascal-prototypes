{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Timestamps;

{$mode objfpc}{$H+}

interface

function Miliseconds(FirstTickCount : Cardinal) : string; inline;

implementation

uses SysUtils;

function Miliseconds(FirstTickCount : Cardinal) : string;
begin
  Result := IntToStr(GetTickCount64 - FirstTickCount);
end;

end.
