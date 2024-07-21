      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1473.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1473.CPv  $
      *
      *   Rev 2.1   Jan 20 2003 13:08:54   t13ex48
      *AKI00303 KOKUE
      *
      *   Rev 2.0   Oct 16 2002 08:30:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  DRKI1473-SATZ.
           05 DRKI1473-KUNDEN-ID           PIC  X(013).
           05 DRKI1473-KOMMBETR-ID         PIC  X(013).
      * Laufende Nummer des komm. Betreuers
      * - 0 es gibt nur einen Vorschlag
      * - 1 erster Vorschlag bei Mehrfachbetreuung
      * - 2 Alternativ-Vorschläge bei Mehrfachbetreuung
           05 DRKI1473-KOMMBETR-LFD        PIC 9(001).
      * Direktion des kommisarischen Betreuers
           05 DRKI1473-DIR                 PIC  9(003).
           05 DRKI1473-ANZ-KKB             PIC S9(004) COMP.

