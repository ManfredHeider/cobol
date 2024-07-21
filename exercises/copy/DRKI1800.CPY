      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1800.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1800.CPv  $
      *
      *   Rev 2.3   Aug 14 2020 10:56:08   t13ac20
      *AKI01795: One Company - Umsetzung der Stufe 3 AM-Leben -> Ge
      *nerali Deutschland Lebensversicherung - Nacharbeiten
      *
      *   Rev 2.2   Mar 09 2017 17:45:20   t13ac14
      *AKI01492: AML-Änderungen zum RT 03/2017-Riester
      *
      *   Rev 2.1   Oct 18 2005 11:40:08   t13ex49
      *AKI00483: Riester Förderung - Sourcen und OPC
      *
      *   Rev 2.0   May 17 2005 12:41:16   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * AML - Riesterzulage DTA
      *
      * Logbuch
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00483!17.05.05!    ! NEU                       !Peter Pohl*
      * -------------------------------------------------------------*
      * AKI01492!22.02.17!2.2 ! neue Felder: KUERZ-SL     !von Helden*
      *         !        !    ! und KEIN-ANSPR-SL;        !          *
      *         !        !    ! Änderung Länge Res.-Filler!          *
      *         !        !    ! in Vor- und Nachsatz;     !          *
      *         !        !    ! Vorsatz redefiniert Haupt-!          *
      *         !        !    ! satz, Nachsatz den Vorsatz!          *
      * -------------------------------------------------------------*
      * AKI01795!14.08.20!2.3 ! Änderung ABSENDER         !T13AC20   *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
       01  AML-AENDERUNG-SATZ.
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
           10 AML-SATZ.
              20 AML-SA                          PIC X(01).
      *          SATZART        = 'C'
              20 AML-VNR                         PIC 9(10).
      *          VERTRAGSNR.
              20 AML-Zul-Datum                   PIC 9(08).
      *          Eingang der Zulage bzw. Zulagekorrektur JJJJMMTT
              20 AML-Zul-Beitragsjahr            PIC 9(04).
      *          Beitragsjahr der Zulage JJJJ
              20 AML-Zul-Wert                    PIC S9(13)V99.
      *          Höhe der Zulage
              20 AML-WKZ                         PIC X(03).
      *          Währungskennzeichen
              20 AML-KUERZ-SL                    PIC X(02).
      *          Kürzungsschlüssel
              20 AML-KEIN-ANSPR-SL               PIC X(02).
      *          Anspruchschlüssel
      *
      ***********************************************************
      *                                                         *
      *         VORSATZ                                         *
      *                                                         *
      ***********************************************************
           10 AML-VORSATZ REDEFINES AML-SATZ.
              20 AML-V-SA                        PIC X(01).
      *          SATZART = 'A'
              20 AML-V-ABSENDER                  PIC X(04).
      *          ABSENDER = 'GEDL'
              20 AML-V-DATUM                     PIC 9(08).
      *          ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
              20 AML-V-EMPFAENGER                PIC X(04).
      *          EMPFAENGER = 'DVAG'
              20 AML-V-TEXT                      PIC X(06).
      *          ÄNDERUNGSBESTAND = 'Zulage'
              20 FILLER                          PIC X(22).
      *
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
           10 AML-NACHSATZ REDEFINES AML-VORSATZ.
              20 AML-N-SA                        PIC X(01).
      *          SATZART             'E'
              20 AML-N-ANZ                       PIC 9(09).
      *          ANZAHL SAETZE INCL. VOR- UND NACHSATZ
              20 Filler                          PIC X(35).
      *
      *------------------------- Ende DRKI1800 ------------------


