      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5062.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5062.CPv  $
      *
      *   Rev 2.4   Jul 05 2016 07:57:00   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.3   May 18 2016 11:48:06   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Feb 22 2016 14:26:04   t13ac19
      *AMA00277: Existenzgründer-Zuschuss wird zu Existenzgründer-B
      *onus (Neu) - Copys
      *
      *   Rev 2.1   Feb 13 2015 15:27:06   t13ac19
      *AMA00147: Änderung Sonderleistungen 2015 - Copies
      *
      *   Rev 2.0   Feb 02 2015 16:44:56   t13ac19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: BUCHUNG EXISTENZGRÜNDER-BONUS (NEU)
      *
      *     SOURCE-NAME      :  DRMA5062
      *
      *     GENERIERUNG      :  20.06.2016/11:08:38
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00147!12.02.15!2.1  ! NEUERSTELLUNG             ! O.WILHELM
      * AMA00277!22.02.16!2.2  !EXISTENZGRÜNDER-ZUSCHUSS =>! O.WILHELM
      *         !        !     !EXISTENZGRÜNDER-BONUS (NEU)!
      * AMA00270!11.04.16!2.3  !ÄNDERUNG DGMA4345          ! WALBERT
      *         !        !     ! (war nicht produktiv)     !
      * AMA00270!20.06.16!2.4  ! Änderung in DGMA4345      !   Walbert
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5062.
              07 DGMA4345.
                 09 VBNR                pic 9(9).
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 DIR-NR              pic S9(4)      binary.
                 09 KNR-EP-TYP          pic S9(4)      binary.
                 09 EQF                 pic S999V99    packed-decimal.
                 09 WERTUNGSZEITRAUM    pic 9.
                 09 AE-SUMME-UST-FREI   pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-FREI
                                        pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-FREI-EFF
                                        pic S9(10)V9   packed-decimal.
                 09 BRUTTO-BETRAG-UST-FREI
                                        pic S9(9)V99   packed-decimal.
                 09 AUSZAHLUNG-UST-FREI pic S9(9)V99   packed-decimal.
                 09 STORMIND-PROZ       pic S999V99    packed-decimal.
                 09 AE-MONATS-DS-GRENZE pic S9(4)V9    packed-decimal.
                 09 FAKTOR              pic S9V99      packed-decimal.
                 09 BUCHDAT             pic 9(8).
                 09 AE-SUMME-UST-PFL    pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-PFL
                                        pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-PFL-EFF
                                        pic S9(10)V9   packed-decimal.
                 09 BRUTTO-BETRAG-UST-PFL
                                        pic S9(9)V99   packed-decimal.
                 09 AUSZAHLUNG-UST-PFL  pic S9(9)V99   packed-decimal.
      *
      **************************************************************

