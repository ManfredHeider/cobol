      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:34:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: PARAMETER STEUER (DRAT0007)                   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: M. Kuhn                                          * 00060000
      *    DATUM:    27.11.1992                                       * 00070000
      *    STAND:    01.12.1992                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *     In Parameter-Steuer sind die Übergabeparameter an das
      *     Schreibmodul zusammengefaßt.
      *
      *     PS-Programm-KZ          D  =  Dialog
      *                             B  =  Batch
      *
      *     PS-Aend-KZ              U  =  Update
      *                             I  =  Insert
      *                             D  =  Delete

       01   Parameter-Steuer.
         05  PS-Programm-ID       PIC  X(008).
         05  PS-Programm-KZ       PIC  X(001).
         05  PS-User-ID           PIC  X(008).
         05  PS-Terminal-ID       PIC  X(008).
         05  PS-Date-Aend         PIC  X(010).
         05  PS-Date-Von          PIC  X(010).
         05  PS-Aend-KZ           PIC  X(001).


      *     In Parameter-Daten sind die Arbeitsdaten an das Schreib-
      *     modul zusammengefaßt. Hier muß der Platz für die Bereiche
      *     reserviert werden.
      *     Die Größe von Tupel kann über Workbench (Table/Cols)
      *     übernommen werden, abzüglich 2 Byte für jedes Varchar und
      *     1 Byte für jedes NULL Attribut, zuzüglich Anzahl Attribute
      *     * 2 (für die NULL-Indikator Tabelle).

      *     Die Parameter-Daten sind im Programm zu definieren.
      *     Diese hängen von der betroffenen Table ab.

      *01   Parameter-Daten.
      *  05  PD-Last-Update       PIC  X(010).
      *  05  PD-Tupel             PIC  X(nnn).

      *     In Parameter-Return sind die Übergabeparameter an das
      *     rufende Programm zusammengefaßt.
      *     Nur wenn Return_Code ungleich Null ist, muß Fehler-Typ
      *     und Fehler-Nr ausgewertet werden.
      *
      *     PR-Fehler-Typ           L  =  Parameter-Fehler  (Logik)
      *                             S  =  MVS-System-Fehler
      *                             D  =  SQL-Error
      *                             R  =  SQL-Error, Rollback muß durch
      *                                   rufendes Pgm veranlaßt werden

       01   Parameter-Return.
         05  PR-Return-Code       PIC  X(001).
         05  PR-Fehler-Typ        PIC  X(001).
         05  PR-Programm-ID       PIC  X(008).
         05  PR-Fehler-Nr         PIC S9(004)    COMP.
         05  PR-Fehler-Text       PIC  X(070).
