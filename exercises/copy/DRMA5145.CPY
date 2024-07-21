      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5145.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5145.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:31:26   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   May 18 2016 11:25:58   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   May 04 2016 11:38:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5045   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5145
      *
      *     GENERIERUNG      :  02.10.2017/14:07:43
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00270!04.05.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DGMA5009 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5145.
              07 DGAT0102.
                 09 DGAT0100.
                    11 FUNKTION         pic X(8).
                 09 OPERATION           pic XXX.
           88 KST-SELECT                value 'SEL'.
           88 KST-INSERT                value 'INS'.
           88 KST-UPDATE                value 'UPD'.
           88 KST-DELETE                value 'DEL'.
           88 KST-START                 value 'ST '.
           88 KST-BACKWARD              value 'BW '.
           88 KST-FORWARD               value 'FW '.
           88 KST-OPEN                  value 'OPE'.
           88 KST-CLOSE                 value 'CLO'.
           88 KST-FETCH                 value 'FET'.
              07 VBNR                   pic 9(9).
              07 FACH-KENNUNG           pic X(10).
              07 BUCHDAT                pic X(10).
              07 BUCHTEXT               pic X(92).
              07 DGMA5009.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
              07 DGMA5009-KU.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
              07 DGMA5009-U.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
      *
      **************************************************************

