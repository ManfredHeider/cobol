      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0317.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WDKI0317.CPv  $
      *
      *   Rev 2.3   Oct 17 2005 08:57:26   t13ex27
      *AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
      *
      *   Rev 2.2   May 24 2004 10:53:10   t13ex27
      *DB-Änderung für Fix 61 EA-Programme und WDKI's
      *
      *   Rev 2.1   Apr 16 1999 10:07:30   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:30   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HAFTPFLICHT-GESAMT.
         10 EA-NI-HAFTPFLICHT-TAB.
           20 EA-NI-HAFTPFLICHT-ELE PIC X OCCURS 44 TIMES.
         10 EA-NI-HAFTPFLICHT
                 REDEFINES EA-NI-HAFTPFLICHT-TAB.
           20 EA-NI-HPFVERTRAGSID            PIC X.
           20 EA-NI-HPFVERSTARIFLFDNR        PIC X.
           20 EA-NI-HPFGRUNDZUSATZKZ         PIC X.
           20 EA-NI-HPFVERSSUMMEEK           PIC X.
           20 EA-NI-HPFVSEKWKZ               PIC X.
           20 EA-NI-HPFVERSSUMMEPERSON       PIC X.
           20 EA-NI-HPFVSPERSONWKZ           PIC X.
           20 EA-NI-HPFVERSSUMMESACH         PIC X.
           20 EA-NI-HPFVSSACHWKZ             PIC X.
           20 EA-NI-HPFVERSSUMMIETSACH       PIC X.
           20 EA-NI-HPFVSMIETSACHWKZ         PIC X.
           20 EA-NI-HPFVERSSUMMEVERM         PIC X.
           20 EA-NI-HPFVSVERMOEGWKZ          PIC X.
           20 EA-NI-HPFLEBENSPARTNERKZ       PIC X.
           20 EA-NI-HPFSELBSTBETEILEK        PIC X.
           20 EA-NI-HPFSBEKWKZ               PIC X.
           20 EA-NI-HPFSELBSTBETVERM         PIC X.
           20 EA-NI-HPFSBVERMOEGWKZ          PIC X.
           20 EA-NI-HPFSELBSTBETEILMS        PIC X.
           20 EA-NI-HPFSBMIETSACHWKZ         PIC X.
           20 EA-NI-HPFSELBSTBETPERSON       PIC X.
           20 EA-NI-HPFSBPERSONWKZ           PIC X.
           20 EA-NI-HPFSELBSTBETEILS         PIC X.
           20 EA-NI-HPFSBSACHWKZ             PIC X.
           20 EA-NI-HPFJAHRESBEITRAG         PIC X.
           20 EA-NI-HPFJAHRESBEITRWKZ        PIC X.
           20 EA-NI-HPFPRODUKTNUMMER         PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-HPFMITVERSELTERNT        PIC X.
           20 EA-NI-HPFANZMITVERSELT         PIC X.
           20 EA-NI-HPFMITVERSKIND           PIC X.
           20 EA-NI-HPFANZMITVERSKIND        PIC X.
           20 EA-NI-HPFAUSFDHAUPTRISK        PIC X.
           20 EA-NI-HPFAUSFDELTERN           PIC X.
           20 EA-NI-HPFAUSFDKIND             PIC X.
           20 EA-NI-HPFANZAHLHUNDE           PIC X.
           20 EA-NI-HPFANZAHLPFERDE          PIC X.
           20 EA-NI-HPFANZAHLEGW             PIC X.
           20 EA-NI-HPFANZAHLMBOHNEV         PIC X.
           20 EA-NI-HPFANZAHLMBMITV          PIC X.
           20 EA-NI-HPFANZAHLSBOHNEV         PIC X.
           20 EA-NI-HPFANZAHLSBMITV          PIC X.
           20 EA-NI-HPFZUKNETTOJBR           PIC X.
           20 EA-NI-HPFZUKNETTOJBRWKZ        PIC X.
         10 EA-DCL-HAFTPFLICHT.
           20 EA-HPFVERTRAGSID               PIC X(13).
           20 EA-HPFVERSTARIFLFDNR           PIC -----9.
           20 EA-HPFGRUNDZUSATZKZ            PIC X(1).
           20 EA-HPFVERSSUMMEEK              PIC ---------9.99.
           20 EA-HPFVSEKWKZ                  PIC X(1).
           20 EA-HPFVERSSUMMEPERSON          PIC ---------9.99.
           20 EA-HPFVSPERSONWKZ              PIC X(1).
           20 EA-HPFVERSSUMMESACH            PIC ---------9.99.
           20 EA-HPFVSSACHWKZ                PIC X(1).
           20 EA-HPFVERSSUMMIETSACH          PIC ---------9.99.
           20 EA-HPFVSMIETSACHWKZ            PIC X(1).
           20 EA-HPFVERSSUMMEVERM            PIC ---------9.99.
           20 EA-HPFVSVERMOEGWKZ             PIC X(1).
           20 EA-HPFLEBENSPARTNERKZ          PIC X(1).
           20 EA-HPFSELBSTBETEILEK           PIC ---------9.99.
           20 EA-HPFSBEKWKZ                  PIC X(1).
           20 EA-HPFSELBSTBETVERM            PIC ---------9.99.
           20 EA-HPFSBVERMOEGWKZ             PIC X(1).
           20 EA-HPFSELBSTBETEILMS           PIC ---------9.99.
           20 EA-HPFSBMIETSACHWKZ            PIC X(1).
           20 EA-HPFSELBSTBETPERSON          PIC ---------9.99.
           20 EA-HPFSBPERSONWKZ              PIC X(1).
           20 EA-HPFSELBSTBETEILS            PIC ---------9.99.
           20 EA-HPFSBSACHWKZ                PIC X(1).
           20 EA-HPFJAHRESBEITRAG            PIC ---------9.99.
           20 EA-HPFJAHRESBEITRWKZ           PIC X(1).
           20 EA-HPFPRODUKTNUMMER            PIC -----9.
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-HPFMITVERSELTERNT           PIC X(40).
           20 EA-HPFANZMITVERSELT            PIC -----9.
           20 EA-HPFMITVERSKIND              PIC X(40).
           20 EA-HPFANZMITVERSKIND           PIC -----9.
           20 EA-HPFAUSFDHAUPTRISK           PIC X(1).
           20 EA-HPFAUSFDELTERN              PIC X(1).
           20 EA-HPFAUSFDKIND                PIC X(1).
           20 EA-HPFANZAHLHUNDE              PIC -----9.
           20 EA-HPFANZAHLPFERDE             PIC -----9.
           20 EA-HPFANZAHLEGW                PIC -----9.
           20 EA-HPFANZAHLMBOHNEV            PIC -----9.
           20 EA-HPFANZAHLMBMITV             PIC -----9.
           20 EA-HPFANZAHLSBOHNEV            PIC -----9.
           20 EA-HPFANZAHLSBMITV             PIC -----9.
           20 EA-HPFZUKNETTOJBR              PIC ---------9.99.
           20 EA-HPFZUKNETTOJBRWKZ           PIC X(1).
      *****************************************************************
