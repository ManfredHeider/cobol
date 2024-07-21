      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRDB0004.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRDB0004.CPv  $
      *
      *   Rev 2.3   Jul 21 2015 14:52:14   p13sp14
      *ADB00007: DB2 User Defined Functions
      *
      *   Rev 2.2   Jul 21 2015 14:41:24   p13sp14
      *ADB00007: DB2 User Defined Functions
      *
      *   Rev 2.1   Jul 21 2015 14:34:46   p13sp14
      *ADB00007: DB2 User Defined Functions
      *
      *   Rev 2.0   Jul 21 2015 14:13:44   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000100******************************************************************
000200* DB2-HILFSFELDER UND ROUTINEN
000300******************************************************************
000400 01  DSN8408.
000500    03 DSN8408-1.
000600      05 FILLER PIC X(53) VALUE
000700        'DSN8408E - ERROR DETECTED BY MESSAGE FORMAT ROUTINE, '.
000800    03 DSN8408-2.
000900      05 FILLER PIC X(15) VALUE 'RETURNCODE IS: '.
001000      05 TIARCODE8 PIC -(10).
001100      05 FILLER PIC X(13) VALUE 'SQLCODE IS: '.
001200      05 SQLCODE8 PIC -(10).
001300
001400 01 ERROR-TEXT-LEN          PIC S9(9) COMP     VALUE +80.
001500
001600 01 ERROR-MESSAGE.
001700    02 ERROR-LEN PIC S9(4) COMP VALUE +960.
001800    02 ERROR-TEXT PIC X(80) OCCURS 12 TIMES.
001900
002000 01 i-Error     PIC S9(9) COMP.
002100 01 Aktuell-SQL-Information.
002200    05 FILLER PIC X(12) value 'Aktuell SQL:'.
002300    05 Aktuell-SQL PIC X(68).
002400
002500 01 Flag-Programm-Abend-call pic x value 'F'.
002600*   -- Vermeidung recursive call of Error Routine:
002700    88 Programm-Abend-called     value 'T'.
002800    88 Programm-Abend-not-called value 'F'.



