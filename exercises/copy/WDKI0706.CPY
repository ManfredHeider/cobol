      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0706.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0706.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:00   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:10   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ARBEITSAUFTRAG-GESAMT.
         10 EA-NI-ARBEITSAUFTRAG-TAB.
           20 EA-NI-ARBEITSAUFTRAG-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-ARBEITSAUFTRAG
                 REDEFINES EA-NI-ARBEITSAUFTRAG-TAB.
           20 EA-NI-ARBEITSAUFTRAGID         PIC X.
           20 EA-NI-VERTRAGSNUMMER           PIC X.
           20 EA-NI-HYPPERSONENID            PIC X.
           20 EA-NI-HYPPERSONENROLLE         PIC X.
           20 EA-NI-HYPHWELFDNUMMER          PIC X.
           20 EA-NI-HYPLAUFENDENUMMER        PIC X.
           20 EA-NI-ERLEDIGTBISDATUM         PIC X.
           20 EA-NI-GESENDET                 PIC X.
         10 EA-DCL-ARBEITSAUFTRAG.
           20 EA-ARBEITSAUFTRAGID            PIC X(13).
           20 EA-VERTRAGSNUMMER              PIC X(13).
           20 EA-HYPPERSONENID               PIC X(13).
           20 EA-HYPPERSONENROLLE            PIC X(01).
           20 EA-HYPHWELFDNUMMER             PIC --------9 .
           20 EA-HYPLAUFENDENUMMER           PIC --------9 .
           20 EA-ERLEDIGTBISDATUM            PIC X(10).
           20 EA-GESENDET                    PIC X(01).
      ******************************************************************
