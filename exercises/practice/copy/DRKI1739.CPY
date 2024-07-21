      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1739.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1739.CPv  $
      *
      *   Rev 2.1   Mar 31 2005 12:31:18   t13ex48
      *AKI00456: Projekt e-Post: Import und Prüfung der Metadaten d
      *er AML
      *
      *   Rev 2.0   Nov 26 2004 17:09:02   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 7 3 9    ########                    *
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
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1739.

            10  FILLER                        PIC X(001) VALUE '"'.
            10  Epost-OUT-Erstelldat          PIC X(010).
      *            Erstell-Datum im Format ??????????
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-Dok-Art             PIC X(050).
      *            Dokument-Art(Betreff)
            10  FILLER                        PIC X(002) VALUE '";'.
            10  Epost-OUT-Kategorie           PIC 9(002).
            10  FILLER                        PIC X(001) VALUE ';'.
            10  Epost-OUT-Ges-Empf            PIC 9(002).
      *        empfangende Gesellschaft ("22" = DVAG)
            10  FILLER                        PIC X(001) VALUE ';'.
            10  Epost-OUT-Ges-Abs             PIC 9(002).
      *        Liefernde Gesellschaft   ("01" = AML)
            10  FILLER                        PIC X(001) VALUE ';'.
            10  Epost-OUT-Schl-Typ            PIC 9(003).
      *        Schluessel-Typ
            10  FILLER                        PIC X(002) VALUE ';"'.
            10  Epost-OUT-Schl-formatiert     PIC X(018).
      *        aufbereitete Vertrags-Nr.
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-Schluessel          PIC X(018).
      *        Vertrags-Nummer ohne fuehrende Nullen
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-VN-VName            PIC X(030).
      *        Vorname des VN
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-VN-Name             PIC X(070).
      *        Nachname des VN
            10  FILLER                        PIC X(002) VALUE '";'.
            10  Epost-OUT-Konzern-KDNR        PIC 9(015).
      *        Konzern-Kundennummer des VN
            10  FILLER                        PIC X(001) VALUE ';'.
            10  Epost-OUT-VBNR                PIC 9(007).
      *        VB-Nummer des Kundenbetreuers
            10  FILLER                        PIC X(002) VALUE ';"'.
            10  Epost-OUT-Dok-Adr             PIC X(100).
      *        Dokument-Adresse
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-Archiv-Adr          PIC X(100).
      *        Archiv-Adresse
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-Archiv-KZ           PIC X(005).
      *        Archivierungs-KZ: "TRUE" oder "FALSE"
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-SA                  PIC X(006).
      *        Satzart: "INSERT" oder "UPDATE"
            10  FILLER                        PIC X(002) VALUE '";'.
            10  Epost-OUT-Dok-Groesse         PIC 9(008).
      *        Groesse des Dokuments in Byte
            10  FILLER                        PIC X(002) VALUE ';"'.
            10  Epost-OUT-Mime-Type           PIC X(050).
      *        Anwendung, mit der Dokument betrachtet werden kann
            10  FILLER                        PIC X(003) VALUE '";"'.
            10  Epost-OUT-Status-ID           PIC X(001).
      *        Betreuungs-Status des Vertrags
            10  FILLER                        PIC X(002) VALUE '";'.
            10  Epost-OUT-DL                  PIC 9(007).
      *        Direktionsleiter des Vertrags-Betreuers
            10  FILLER                        PIC X(001) VALUE ';'.
            10  FILLER                        PIC X(051).
      *            RESERVE
      *
      *----------- E N D E   D R K I 1 7 3 8 --------------*
