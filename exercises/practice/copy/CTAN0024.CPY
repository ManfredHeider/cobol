      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0024.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/CTAN0024.CPv  $
      *
      *   Rev 2.7   Feb 25 2016 10:10:56   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:22:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:52:06   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:56:20   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 22 2015 15:45:38   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 17:39:44   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:26:36   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Mar 09 2015 14:51:40   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0024                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0024 DVMH             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1  !NEU                        !RAHNER   *
      * AAN01198!07.07.15!2.4  !NEU FELD EINR.GEBÜHR       !RAHNER   *
      * AAN01198!11.08.15!2.5  !VERGRÖßERN FELDER          !RAHNER   *
      * AAN01240!22.02.16!2.6  !FIRMA-NAME-LANG            !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBAN0024)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0024 TABLE
CCC004*    ( DVMH_VNR                       DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      DVMH_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      DVMH_KENNUNG_AEND              CHAR(8) NOT NULL,
CCC004*      DVMH_ERSTELLUNG                TIMESTAMP NOT NULL,
CCC004*      DVMH_KENNUNG_ERST              CHAR(8) NOT NULL,
CCC004*      DVMH_FA_RF                     SMALLINT NOT NULL,
CCC004*      DVMH_FA_N1                     CHAR(30) NOT NULL,
CCC004*      DVMH_FA_N2                     CHAR(30) NOT NULL,
CCC004*      DVMH_FA_STR                    CHAR(45) NOT NULL,
CCC004*      DVMH_FA_PLZ                    CHAR(15) NOT NULL,
CCC004*      DVMH_FA_ORT                    CHAR(50) NOT NULL,
CCC004*      DVMH_FA_POSTF                  CHAR(10) NOT NULL,
CCC004*      DVMH_FA_PLZ_POSTF              CHAR(5) NOT NULL,
CCC004*      DVMH_FA_ORT_POSTF              CHAR(40) NOT NULL,
CCC004*      DVMH_FA_TELE                   CHAR(30) NOT NULL,
CCC004*      DVMH_FA_FAX                    CHAR(30) NOT NULL,
CCC004*      DVMH_FA_MOBIL                  CHAR(30) NOT NULL,
CCC004*      DVMH_FA_EMAIL                  CHAR(50) NOT NULL,
CCC004*      DVMH_FA_KZ_VST                 CHAR(1) NOT NULL,
CCC004*      DVMH_FA_ST_NR                  CHAR(15) NOT NULL,
CCC004*      DVMH_FA_HREG_AMTG              CHAR(50) NOT NULL,
CCC004*      DVMH_FA_HREG_NR                CHAR(30) NOT NULL,
CCC004*      DVMH_FA_INH                    CHAR(100) NOT NULL,
CCC004*      DVMH_KZ_FAC_OPT                CHAR(1) NOT NULL,
CCC004*      DVMH_UMSATZ_JAHR               DECIMAL(9, 0) NOT NULL,
CCC004*      DVMH_KZ_GARANT                 CHAR(1) NOT NULL,
CCC004*      DVMH_KZ_GW_GES                 CHAR(1) NOT NULL,
CCC004*      DVMH_KZ_N_ERKL                 CHAR(1) NOT NULL,
CCC004*      DVMH_KIH_VN                    CHAR(30) NOT NULL,
CCC004*      DVMH_KIH_ZN                    CHAR(30) NOT NULL,
CCC004*      DVMH_KIH_ABW_STR               CHAR(45) NOT NULL,
CCC004*      DVMH_KIH_ABW_PLZ               CHAR(15) NOT NULL,
CCC004*      DVMH_KIH_ABW_ORT               CHAR(50) NOT NULL,
CCC004*      DVMH_GELDINST                  CHAR(58) NOT NULL,
CCC004*      DVMH_BIC                       CHAR(11) NOT NULL,
CCC004*      DVMH_IBAN                      CHAR(34) NOT NULL,
CCC004*      DVMH_SEPA_M_BEG                INTEGER NOT NULL,
CCC004*      DVMH_E_GEBUEHR                 CHAR(1) NOT NULL,
CCC004*      DVMH_FIRMA_LANG                CHAR(70)NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0024                           *
      ******************************************************************
       01  DCLTBAN0024.
           10 DVMH-VNR             PIC S9(15)V COMP-3.
           10 PGEB-GES             PIC S9(4) COMP.
           10 DVMH-LAST-UPDATE     PIC X(26).
           10 DVMH-KENNUNG-AEND    PIC X(8).
           10 DVMH-ERSTELLUNG      PIC X(26).
           10 DVMH-KENNUNG-ERST    PIC X(8).
           10 DVMH-FA-RF           PIC S9(4) COMP.
           10 DVMH-FA-N1           PIC X(30).
           10 DVMH-FA-N2           PIC X(30).
           10 DVMH-FA-STR          PIC X(45).
           10 DVMH-FA-PLZ          PIC X(15).
           10 DVMH-FA-ORT          PIC X(50).
           10 DVMH-FA-POSTF        PIC X(10).
           10 DVMH-FA-PLZ-POSTF    PIC X(5).
           10 DVMH-FA-ORT-POSTF    PIC X(40).
           10 DVMH-FA-TELE         PIC X(30).
           10 DVMH-FA-FAX          PIC X(30).
           10 DVMH-FA-MOBIL        PIC X(30).
           10 DVMH-FA-EMAIL        PIC X(50).
           10 DVMH-FA-KZ-VST       PIC X(1).
           10 DVMH-FA-ST-NR        PIC X(15).
           10 DVMH-FA-HREG-AMTG    PIC X(50).
           10 DVMH-FA-HREG-NR      PIC X(30).
           10 DVMH-FA-INH          PIC X(100).
           10 DVMH-KZ-FAC-OPT      PIC X(1).
           10 DVMH-UMSATZ-JAHR     PIC S9(9)V COMP-3.
           10 DVMH-KZ-GARANT       PIC X(1).
           10 DVMH-KZ-GW-GES       PIC X(1).
           10 DVMH-KZ-N-ERKL       PIC X(1).
           10 DVMH-KIH-VN          PIC X(30).
           10 DVMH-KIH-ZN          PIC X(30).
           10 DVMH-KIH-ABW-STR     PIC X(45).
           10 DVMH-KIH-ABW-PLZ     PIC X(15).
           10 DVMH-KIH-ABW-ORT     PIC X(50).
           10 DVMH-GELDINST        PIC X(58).
           10 DVMH-BIC             PIC X(11).
           10 DVMH-IBAN            PIC X(34).
           10 DVMH-SEPA-M-BEG      PIC S9(9) COMP.
           10 DVMH-E-GEBUEHR       PIC X(1).
           10 DVMH-FIRMA-LANG      PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 40      *
      ******************************************************************




