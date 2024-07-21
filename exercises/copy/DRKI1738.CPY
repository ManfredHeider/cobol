      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1738.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1738.CPv  $
      *
      *   Rev 2.7   Feb 12 2008 14:06:16   t13ex48
      *AKI00683: EDI-Anpassungen für Allfinanz: PMKI0778
      *
      *   Rev 2.6   Feb 12 2008 13:12:52   t13ex48
      *AKI00683: EDI-Anpassungen für Allfinanz: PMKI0778
      *
      *   Rev 2.5   Jun 28 2007 11:05:52   t13ex48
      *AKI00657: AGI-Stornos in EDI anzeigen
      *
      *   Rev 2.4   Aug 31 2006 12:52:42   t13ex48
      *AKI00584: Referenztabelle für EDI-Dokumente
      *
      *   Rev 2.2   Oct 31 2005 10:26:00   t13ex48
      *AKI00518: EDI-Metadatensätze von der AMV
      *
      *   Rev 2.1   Mar 31 2005 12:31:10   t13ex48
      *AKI00456: Projekt e-Post: Import und Prüfung der Metadaten d
      *er AML
      *
      *   Rev 2.0   Nov 26 2004 17:09:02   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 7 3 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  File-Transfer-Datei der AML fuer epost-Meta-Daten             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *26.11.04!NEUERSTELLUNG                          !Sebbach  ! 01  *
      *AKI00456!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *04.10.05! Einbindung AMV                        !Sebbach  ! 02  *
      *AKI00518!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.04.06! Einfuehrung Eingangspost              !Sebbach  ! 03  *
      *AKI00584! Referenztabelle für Betreff-Text      !         !     *
      *        ! Neue Felder                           !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.05.07! Einfuehrung EDI fuer AGI (dit)        !Sebbach  ! 04  *
      *AKI00657!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *31.01.08! Geaenderte Vertrags-Nr AGI PremiumInv !Sebbach  ! 05  *
      *AKI00683!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1738.

            10  Epost-IN-Erstelldat           PIC X(010).
      *            Erstell-Datum im Format JJJJ-MM-TT
            10  Epost-IN-Dok-Art              PIC X(050).
      *            Dokument-Art(Betreff)
            10  Epost-IN-Kategorie            PIC 9(002).
            10  Epost-IN-Ges-Empf             PIC 9(002).
      *        empfangende Gesellschaft ("22" = DVAG)
            10  Epost-IN-Ges-Abs              PIC 9(002).
      *        Liefernde Gesellschaft   ("00" = AMV)
      *                                 ("01" = AML)
            10  Epost-IN-Schl-Typ             PIC 9(003).
      *        Schluessel-Typ
            10  Epost-IN-Schl-formatiert      PIC X(018).
      *        aufbereitete Vertrags-Nr.
            10  Epost-IN-Schluessel           PIC X(018).
      *        Vertrags-Nummer: Laenge variiert je nach Gesellschaft
041005      10  Epost-IN-Schl-AMV      REDEFINES   Epost-IN-Schluessel.
041005       15 Epost-IN-VNR-AMV              PIC 9(012).
041005       15 FILLER                        PIC X(006).
041005      10  Epost-IN-Schl-AML      REDEFINES   Epost-IN-Schluessel.
041005       15 Epost-IN-VNR-AML              PIC 9(010).
041005       15 FILLER                        PIC X(008).
041005*      15 Epost-IN-Schl-num             PIC 9(010).
041005*      15 Epost-IN-Schl-X               PIC X(008).
210507      10  Epost-IN-Schl-AGI      REDEFINES   Epost-IN-Schluessel.
210507       15 Epost-IN-VNR-AGI              PIC 9(009).
210507       15 Epost-IN-UDPT-AGI             PIC 9(002).
210507       15 FILLER                        PIC X(007).
310108      10  Epost-IN-Schl-AGI-PI   REDEFINES   Epost-IN-Schluessel.
310108       15 Epost-IN-VNR-AGI-PI           PIC 9(010).
310108       15 FILLER                        PIC X(008).
            10  Epost-IN-VN-VName             PIC X(030).
      *        Vorname des VN
            10  Epost-IN-VN-Name              PIC X(070).
      *        Nachname des VN
            10  Epost-IN-Konzern-KDNR         PIC 9(015).
      *        Konzern-Kundennummer des VN
            10  Epost-IN-VBNR                 PIC 9(007).
      *        VB-Nummer des Kundenbetreuers
            10  Epost-IN-Dok-Adr              PIC X(100).
      *        Dokument-Adresse
            10  Epost-IN-Archiv-Adr           PIC X(100).
      *        Archiv-Adresse
            10  Epost-IN-Archiv-KZ            PIC X(005).
      *        Archivierungs-KZ: "TRUE" oder "FALSE"
            10  Epost-IN-SA                   PIC X(006).
      *        Satzart: "INSERT" oder "UPDATE"
            10  Epost-IN-Dok-Groesse          PIC 9(008).
      *        Groesse des Dokuments in Byte
            10  Epost-IN-Mime-Type            PIC X(050).
      *        Anwendung, mit der Dokument betrachtet werden kann
190406      10 Epost-IN-CTVBriefTyp           PIC X(016).
>>    *        Brieftyp
            10 Epost-IN-GeVoID                PIC 9(008).
      *        Numerischer Schlüssel für den Geschäftsvorfall
            10 Epost-IN-GeVoKategorieID       PIC 9(008).
      *        Kategorie des Geschäftsvorfalls
            10 Epost-IN-Posttyp               PIC X(001).
      *        Kennzeichnung, ob Ein- oder Ausgangspost
            10 Epost-IN-DokImOriginal         PIC X(005).
      *        Kennzeichnung, ob Dokument im Original an VB ging
            10 Epost-IN-Vorgangsnummer        PIC 9(015).
            10 Epost-IN-PIN-Nr                PIC 9(015).
    <<*     10 FILLER                         PIC X(104).
190406      10 FILLER                         PIC X(036).
      *            RESERVE
      *
      *----------- E N D E   D R K I 1 7 3 8 --------------*
