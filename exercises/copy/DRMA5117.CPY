      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5117.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRMA5117.CPv  $
      *
      *   Rev 2.6   Dec 20 2017 13:25:30   t13ae39
      *AMA00352: UST-Korrekturen der Vorjahre für Ust-pfl. Ges.
      *
      *   Rev 2.5   Mar 29 2017 14:52:22   t13ex48
      *AMA00342: Neue Partnergesellschaft TeleSon (Ges.94): HSE Sou
      *rcen Teleson
      *
      *   Rev 2.4   Mar 29 2017 13:23:16   t13ex48
      *AMA00342: Neue Partnergesellschaft TeleSon (Ges.94): HSE Sou
      *rcen Teleson
      *
      *   Rev 2.3   Jul 20 2016 13:44:48   t13ac17
      *AMA00266: USt: LUCY II - KOMA0507-Statistiken - Copies
      *
      *   Rev 2.2   Jul 04 2016 08:54:24   t13ac17
      *AMA00266: USt: LUCY II - KOMA0507-Statistiken - Copies
      *
      *   Rev 2.1   Oct 26 2015 14:17:08   t13ac13
      *AMA00200: USt: KOMA0507-Statistiken - Teil 2 - Copies
      *
      *   Rev 2.0   Jul 31 2015 11:08:26   t13ac13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSBESTAND FÜR UST.-KORREKTUREN
      *
      *     SOURCE-NAME      :  DRMA5117
      *
      *     GENERIERUNG      :  06.03.2017/12:50:09
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00200!28.09.15!2.1  ! NEUERSTELLUNG             ! BOOST
      * AMA00266!17.05.16!2.2  ! ERWEITERUNGEN AUS LUCY 2  ! WALBERT
      * AMA00266!11.07.16!2.3  ! ERWEITERUNGEN UM BEW-ERST.! WALBERT
      *         !        !     ! UND KNR-VERARB-TYP        !
      * --------!--------!-----!---------------------------!----------
      * AMA00342!23.02.17!2.4  ! ERWEITERUNG UM:           ! HSE
      *         !        !2.5  ! FAELLIG-VON UND PG        !
      * --------!--------!-----!---------------------------!----------
      * AMA00360!07.11.17!2.6  ! ERWEITERUNG FOLGE-NEU     ! Strauss
      * AMA00352!        !     ! WG. PRODART               !
      * --------!--------!-----!---------------------------!----------
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 ARBEITSBESTAND.
              07 VBNR                   pic 9(9).
              07 PROV-AUS-KU            pic S9(9)V99   packed-decimal.
              07 PROV-AUS-U             pic S9(9)V99   packed-decimal.
              07 PROV-UST               pic S9(9)V99   packed-decimal.
              07 UST-SATZ               pic S999V99    packed-decimal.
              07 ABR-NR-KORR            pic X(15).
              07 ABR-NR-AKT             pic X(15).
              07 PG-RECH-NR             pic X(12).
              07 KORREKTUR-ZIEL-STATUS  pic X.
              07 STEUERJAHR             pic X(4).
              07 VERTRAGS-ID            pic X(15).
              07 PRODSCHL               pic 999.
071117        07 PRODART                pic 99.
              07 BUCHTEXT               pic X(92).
              07 DV-KENNUNG             pic X(10).
              07 REFERENZ-NR            pic X(16).
              07 FIBU-SORT-ID           pic 9(4).
              07 FIBU-SORT-LFDNR        pic 9(4).
              07 BEW-ERSTELL-ZPKT       pic X(26).
              07 KNR-VERARB-TYP         pic S9(4)      binary.
              07 FAELLIG-VON            pic X(10).
              07 PG                     pic 99.
      *
      **************************************************************
 
 

