      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0703.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0703.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:54   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ORGDOKUMENT-GESAMT.
         10 EA-NI-ORGDOKUMENT-TAB.
           20 EA-NI-ORGDOKUMENT-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-ORGDOKUMENT
                 REDEFINES EA-NI-ORGDOKUMENT-TAB.
           20 EA-NI-ORGDOCID                 PIC X.
           20 EA-NI-SUBTYPEORGDOC            PIC X.
           20 EA-NI-PRIORITAETID             PIC X.
           20 EA-NI-ARTID                    PIC X.
           20 EA-NI-MITSTRUKTUR              PIC X.
           20 EA-NI-DATUM                    PIC X.
           20 EA-NI-UHRZEITVON               PIC X.
           20 EA-NI-UHRZEITBIS               PIC X.
           20 EA-NI-BESCHREIBUNG             PIC X.
           20 EA-NI-GRUNDID                  PIC X.
         10 EA-DCL-ORGDOKUMENT.
           20 EA-ORGDOCID                    PIC X(13).
           20 EA-SUBTYPEORGDOC               PIC X(4).
           20 EA-PRIORITAETID                PIC --------9 .
           20 EA-ARTID                       PIC --------9 .
           20 EA-MITSTRUKTUR                 PIC X(01).
           20 EA-DATUM                       PIC X(10).
           20 EA-UHRZEITVON                  PIC X(08).
           20 EA-UHRZEITBIS                  PIC X(08).
           20 EA-BESCHREIBUNG.
              49 EA-BESCHREIBUNG-LEN         PIC -----9    .
              49 EA-BESCHREIBUNG-TEXT        PIC X(200).
           20 EA-GRUNDID                     PIC --------9 .
      ******************************************************************
