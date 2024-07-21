      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0107.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0107.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:52:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 May 1998 10:33:50   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:47:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
000100*         *-----------------------------------------------------*
000200*         ! control-record
000300*         *-----------------------------------------------------*
000400* z-coin-rec als Übergabeparamter in LINKAGE SECTION muß Stufe 01
000500* sein
000600*
000700*  Modified: mta, 2 Dez- for z-coin-extension
000800*  Modified: mta, 4 Dez- für von- bis- vertrag
000900*
001000*01  drki0107-gesamt.
001100*   03 z-coin-rec.
001200 01 z-coin-rec.
001300    05 z-COIN-KGES               PIC 99.
001400* für Einmalige Aktualisierung wird dieser Feld mit 'E' befüllt
001401* für Komplettbestand mit 'J'
001402    05 z-COIN-FULL-LOAD-FLAG     PIC X.
001500    05 z-COIN-RECORD-FLAG-OCC    OCCURS 46.
001600        10 z-COIN-RECORD-FLAG    PIC X(1).
001601* Sind die Vertraege vorhanden J/N
001602    05 z-coin-VERTRAG-VORHANDEN  pic x(1).
001700    05 z-coin-von-vertrag        pic 9(15).
001800    05 z-coin-bis-vertrag        pic 9(15).
001900*      For some parameters to be returned
002000    05 z-coin-extension.
002100       10  z-coin-suchname1-cnt  pic s9(9) comp.
002200       10  z-coin-suchname2-cnt  pic s9(9) comp.
