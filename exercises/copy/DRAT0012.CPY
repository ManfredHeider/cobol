      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:35:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: PARAMETER STEUER (DRAT0012) Wiedervorlage WV  *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. Kuhn                                          *
      *    DATUM:    23.04.1993                                       *
      *    STAND:    23.04.1993                                       *
      *****************************************************************
      *
      *     In Parameter-Steuer sind die Übergabeparameter an das
      *     Schreibmodul Wiedervorlage zusammengefaßt.
      *
      *     PS-Programm-KZ          D  =  Dialog
      *                             B  =  Batch
      *
      *     PS-Aend-KZ              I  =  Insert
      *                             D  =  Delete

       01   Parameter-Steuer-WV.
         05  PS-WV-Programm-ID    PIC  X(008).
         05  PS-WV-Programm-KZ    PIC  X(001).
         05  PS-WV-User-ID        PIC  X(008).
         05  PS-WV-Terminal-ID    PIC  X(008).
         05  PS-WV-Dat-Aend       PIC  X(010).
         05  PS-WV-Dat-Von        PIC  X(010).
         05  PS-WV-Aend-KZ        PIC  X(001).


      *     In Parameter-Daten sind die Arbeitsdaten an das Schreib-
      *     modul zusammengefaßt.
      *
      *     In Table-ID wird der Quell-Table mitgegeben.
      *     In MA-Nr ist die Mitarbeiternummer vermerkt.
      *     In Feldname wird der CICS-Feldname übermittelt.
      *     In Feldtyp wird die Art des Feldes übergeben.
      *                I    =    Integer
      *                S    =    Smallinteger
      *                Z    =    Decimal
      *                D    =    Date
      *                X    =    Character
      *     In Feldlang wird die Länge des Feldes übertragen,
      *     diese muß größer Null und bei Feldtyp 'I'  vier
      *     bzw. bei 'S'  zwei sein.
      *     Bei 'Z' für Decimal muß Feldlang > 0 und <= 10
      *     sein (max 18 Stellen unter Cobol).
      *     Für Date muß die Länge 10 Byte betragen und in Feldwert
      *     muß an den Stellen 5 und 8 ein '-' stehen (Date im
      *     Display-Format = JJJJ-MM-TT).
      *     In Aend-Wert steht der eigentliche neue Wert in
      *     der Länge von Feldlang. Damit der Wert beim Über-
      *     tragen in Aend-Wert nicht konvertiert wird, ist der
      *     eigentliche Bereich als FILLER definiert.

       01   Parameter-Daten-WV.
         05  PD-WV-Table-ID          PIC  X(008).
         05  PD-WV-MA-Nr             PIC S9(009)    COMP.
         05  PD-WV-Feldname          PIC  X(016).
         05  PD-WV-Feldtyp           PIC  X(001).
         05  PD-WV-Aend-Lang         PIC S9(009)    COMP.
         05  PD-WV-Aend-Wert.
           10  FILLER                PIC  X(070).

      *     In Parameter-Return sind die Übergabeparameter an das
      *     rufende Programm zusammengefaßt.
      *     Nur wenn Return-Code ungleich Null ist, muß Fehler-Typ
      *     und Fehler-Nr ausgewertet werden.
      *
      *     PR-Fehler-Typ           L  =  Parameter-Fehler  (Logik)
      *                             S  =  MVS-System-Fehler
      *                             D  =  SQL-Error

       01   Parameter-Return-WV.
         05  PR-WV-Return-Code       PIC  X(001).
         05  PR-WV-Fehler-Typ        PIC  X(001).
         05  PR-WV-Programm-ID       PIC  X(008).
         05  PR-WV-Fehler-Nr         PIC S9(004)    COMP.
         05  PR-WV-Fehler-Text       PIC  X(070).

CCC009*    EJECT
