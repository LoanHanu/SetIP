{===============================================================================

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above. If you wish to
allow use of your version of this file only under the terms of the GPL and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL. If you do not delete the provisions
above, a recipient may use your version of this file under either the MPL or
the GPL.

$Id: ModBusCompiler.inc,v 1.13 2014/10/07 13:34:30 plpolak Exp $

===============================================================================}

{ Logic to detect the used Delphi compiler version: }


{$IFDEF VER120}
  {$DEFINE DMB_DELPHI1}
  {$DEFINE DMB_DELPHI2}
  {$DEFINE DMB_DELPHI3}
  {$DEFINE DMB_DELPHI4}
  {$DEFINE DMB_DELPHI4_ONLY}
{$ENDIF}
{$IFDEF VER130}
    {$DEFINE DMB_DELPHI1}
    {$DEFINE DMB_DELPHI2}
    {$DEFINE DMB_DELPHI3}
    {$DEFINE DMB_DELPHI4}
    {$DEFINE DMB_DELPHI5}
    {$DEFINE DMB_DELPHI5_ONLY}
{$ENDIF}
{$IFDEF VER140}
      {$DEFINE DMB_DELPHI1}
      {$DEFINE DMB_DELPHI2}
      {$DEFINE DMB_DELPHI3}
      {$DEFINE DMB_DELPHI4}
      {$DEFINE DMB_DELPHI5}
      {$DEFINE DMB_DELPHI6}
      {$DEFINE DMB_DELPHI6_ONLY}
{$ENDIF}
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 15.0 }
        {$DEFINE DMB_DELPHI1}
        {$DEFINE DMB_DELPHI2}
        {$DEFINE DMB_DELPHI3}
        {$DEFINE DMB_DELPHI4}
        {$DEFINE DMB_DELPHI5}
        {$DEFINE DMB_DELPHI6}
        {$DEFINE DMB_DELPHI7}
{$IFEND}
      {$IF CompilerVersion >= 17.0 }
        {$DEFINE DMB_DELPHI2005}
      {$IFEND}
      {$IF CompilerVersion >= 18.0 }
        {$DEFINE DMB_DELPHI2006}
      {$IFEND}
      {$IF CompilerVersion >= 18.5 }
         {$DEFINE DMB_DELPHI2007}
      {$IFEND}
      {$IF CompilerVersion >= 20.0 }
         {$DEFINE DMB_DELPHI2009}
      {$IFEND}
      {$IF CompilerVersion >= 21.0 }
         {$DEFINE DMB_DELPHI2010}
      {$IFEND}
      {$IF CompilerVersion >= 22.0}
         {$DEFINE DMB_DELPHIXE}
      {$IFEND}
      {$IF CompilerVersion >= 23.0}
         {$DEFINE DMB_DELPHIXE2}
      {$IFEND}
      {$IF CompilerVersion >= 24.0}
         {$DEFINE DMB_DELPHIXE3}
       {$LEGACYIFEND ON} //remove IFEND warning
      {$IFEND}
      {$IF CompilerVersion >= 25.0}
         {$DEFINE DMB_DELPHIXE4}
      {$IFEND}
      {$IF CompilerVersion >= 26.0}
         {$DEFINE DMB_DELPHIXE5}
      {$IFEND}
      {$IF CompilerVersion >= 27.0}
         {$DEFINE DMB_DELPHIXE6}
      {$IFEND}
      {$IF CompilerVersion >= 28.0}
         {$DEFINE DMB_DELPHIXE7}
      {$IFEND}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE DMB_DELPHI7_ONLY}
{$ENDIF}
{$IFDEF VER170}
   {$DEFINE DMB_DELPHI2005_ONLY}
{$ENDIF}
{$IFDEF VER180}
  {$IFDEF VER185}
    {$DEFINE DMB_DELPHI2007_ONLY}
  {$ELSE}
    {$DEFINE DMB_DELPHI2006_ONLY}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER200}
  {$DEFINE DMB_DELPHI2009_ONLY}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE DMB_DELPHI2010_ONLY}
{$ENDIF}
{$IFDEF VER220}
  {$DEFINE DMB_DELPHIXE_ONLY}
{$ENDIF}
{$IFDEF VER230}
  {$DEFINE DMB_DELPHIXE2_ONLY}
{$ENDIF}
{$IFDEF VER240}
  {$DEFINE DMB_DELPHIXE3_ONLY}
{$ENDIF}
{$IFDEF VER250}
  {$DEFINE DMB_DELPHIXE4_ONLY}
{$ENDIF}
{$IFDEF VER260}
  {$DEFINE DMB_DELPHIXE5_ONLY}
{$ENDIF}
{$IFDEF VER270}
{$DEFINE DMB_DELPHIXE6_ONLY}
{$ENDIF}
{$IFDEF VER280}
  {$DEFINE DMB_DELPHIXE7_ONLY}
{$ENDIF}


{$IFDEF DMB_DELPHI2005}
{ By default use Indy 10 starting from Delphi 2005 }
  {$DEFINE DMB_INDY10}
{$ELSE}
{ Older Delphi versions use Indy 9 }
  {$DEFINE DMB_INDY9}
{$ENDIF}
  
{$IFDEF FPC}
{ Force the Free Pascal Compiler in Delphi mode, and use Indy 10 }
  {$MODE DELPHI}
  {$UNDEF DMB_INDY9}
  {$DEFINE DMB_INDY10}
{$ENDIF}

{ Allow user defines to overrule the Indy version being used }
{$IFDEF FORCE_INDY9}
  {$UNDEF DMB_INDY10}
  {$DEFINE DMB_INDY9}
{$ELSE}
  {$IFDEF FORCE_INDY10}
    {$UNDEF DMB_INDY9}
    {$DEFINE DMB_INDY10}
  {$ENDIF}
{$ENDIF}
