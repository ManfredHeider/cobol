      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0070.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:48   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-JURISTISCHEPERSON-GESAMT.
         10 EA-NI-JURISTISCHEPERSON-TAB.
           20 EA-NI-JURISTISCHEPERSON-ELE PIC X
                  OCCURS 12 TIMES.
         10 EA-NI-JURISTISCHEPERSON
                 REDEFINES EA-NI-JURISTISCHEPERSON-TAB.
           20 EA-NI-JUPPERSONENID            PIC X.
           20 EA-NI-JUPBERECHGEBUEHR         PIC X.
           20 EA-NI-JUPBERGEBUEHRWKZ         PIC X.
           20 EA-NI-JUPERFASSTAM             PIC X.
           20 EA-NI-JUPJAHRESUMSATZ          PIC X.
           20 EA-NI-JUPJAHRUMSATZWKZ         PIC X.
           20 EA-NI-JUPEINKOSTEUER           PIC X.
           20 EA-NI-JUPKIRCHENSTEUER         PIC X.
           20 EA-NI-JUPKOERPERSTEUER         PIC X.
           20 EA-NI-JUPGEWERBESTEUER         PIC X.
           20 EA-NI-JUPGESAMTSTEUER          PIC X.
           20 EA-NI-JUPRECHTSFORMID          PIC X.
         10 EA-DCL-JURISTISCHEPERSON.
           20 EA-JUPPERSONENID               PIC X(13).
           20 EA-JUPBERECHGEBUEHR            PIC ---------9.99    .
           20 EA-JUPBERGEBUEHRWKZ            PIC X(01).
           20 EA-JUPERFASSTAM                PIC X(10).
           20 EA-JUPJAHRESUMSATZ             PIC ---------9.99    .
           20 EA-JUPJAHRUMSATZWKZ            PIC X(01).
           20 EA-JUPEINKOSTEUER              PIC ---9.99          .
           20 EA-JUPKIRCHENSTEUER            PIC ---9.99          .
           20 EA-JUPKOERPERSTEUER            PIC ---9.99          .
           20 EA-JUPGEWERBESTEUER            PIC ---9.99          .
           20 EA-JUPGESAMTSTEUER             PIC ---9.99          .
           20 EA-JUPRECHTSFORMID             PIC -----9    .
      ******************************************************************
