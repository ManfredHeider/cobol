      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2006.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2006.CPv  $
      *
      *   Rev 2.5   14 Apr 2008 16:34:32   T13EX32
      *AKI00713: Besuchsaufträge in der KI PGM PMKI0813 anpassen
      *
      *   Rev 2.4   Dec 31 2007 16:28:00   t13ex48
      *AKI00683: EDI-Anpassungen für Allfinanz: PMKI0813
      *
      *   Rev 2.3   Dec 28 2007 15:09:30   t13ex48
      *AKI00683: EDI-Anpassungen für Allfinanz
      *
      *   Rev 2.2   Dec 28 2007 13:00:50   t13ex48
      *AKI00683: EDI-Anpassungen für Allfinanz: Host-Programme
      *
      *   Rev 2.1   Mar 29 2007 17:34:58   t13ex48
      *AKI00629: Besuchsauftragsverfahren für Online-KI
      *
      *   Rev 2.0   Jan 16 2007 18:25:50   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 0 0 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  File-Transfer-Datei der AML + AMV      fuer BA-Meta-Daten     *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.01.07!NEUERSTELLUNG                          ! Sebbach ! 01  *
      *AKI00629!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
201207*20.12.07! Einfuehrung Allfinanz. Neues Feld:    ! Sebbach ! 02  *
201207*AKI00683! "empfangende Gesellschaft"            !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.12.07! Zurückdrehen letzte Aend.             ! Sebbach ! 03  *
      *AKI00683! (Kopiert aus Rev 2.1)                 !         !     *
      *--------!---------------------------------------!---------!-----*
311207*31.12.07! Aenderungen vom 20.12.07 reaktiviert  ! Sebbach ! 04  *
311207*AKI00683!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
100408*10.04.08! Zusätzliche Felder eingefügt          ! Bößhar  ! 05  *
100408*AKI00713!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI2006.

            10  BA-IN-Ordnungsbegriff         PIC X(027).
      *            Ordnungsbegriff (numerischer Inhalt)
            10  BA-IN-Erstelldat              PIC X(010).
      *            Erstell-Datum im Format JJJJ-MM-TT
            10  BA-IN-Lfd-Nr                  PIC 9(008).
      *            Laufende Nummer
            10  BA-IN-Ges-Abs                 PIC 9(002).
      *            Liefernde Gesellschaft   ("00" = AMV)
      *                                     ("01" = AML)
201207      10  BA-IN-Ges-Empf                PIC 9(002).
201207*            Empfangende Gesellschaft ("22" = DVAG)
201207*                                     ("66" = Allfinanz)
            10  BA-IN-VBNR                    PIC 9(007).
      *        VB-Nummer des Kundenbetreuers
            10  BA-IN-Erl-Dat                 PIC X(010).
      *            Erledigungs-Datum im Format JJJJ-MM-TT
      *     10  BA-IN-Ges-Empf                PIC 9(002).
      *        empfangende Gesellschaft ("22" = DVAG)
            10  BA-IN-Schluessel              PIC 9(018).
      *        Vertrags-Nummer:  (18-stellig mit führenden Nullen)
            10  BA-IN-VN-VName                PIC X(025).
      *        Vorname des VN
            10  BA-IN-VN-Name                 PIC X(040).
      *        Nachname des VN
            10 BA-IN-Vorgangs-Nr              PIC 9(015).
      *        Vorgangs-Nummer
            10 BA-IN-GeVoID                   PIC 9(008).
      *        Numerischer Schlüssel für den Geschäftsvorfall
            10 BA-IN-GeVoKategorieID          PIC 9(008).
      *        Kategorie des Geschäftsvorfalls
            10 BA-IN-Aktivtyp                 PIC X(008).
      *       Grund des Besuchsauftrags
100408      10 BA-IN-Haftungsbeginn           PIC X(010).
      *        Haftungsbeginn
100408      10 BA-IN-Haftungsende             PIC X(010).
      *        Haftungsende
100408      10 BA-IN-Gesamtversicherungssumme PIC 9(015).
      *        Gesamtversicherungssumme
100408      10 FILLER                         PIC X(029).
      *     10 FILLER                         PIC X(064).
      *            RESERVE
      *
      *----------- E N D E   D R K I 1 7 3 8 --------------*
