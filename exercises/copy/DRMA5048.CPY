      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.09.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5048.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5048.CPv  $
      *
      *   Rev 2.3   Sep 20 2018 15:03:42   t13ex15
      *AMA00400: Generali Allfinanz AG Copy Einbehalt
      *
      *   Rev 2.2   Oct 19 2017 14:26:30   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Sep 17 2014 17:23:30   t13ac14
      *AMA00078: Haftungszeitreduzierung Central / neuer PGV
      *
      *   Rev 2.0   Sep 11 2014 16:13:52   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5015   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5048
      *
      *     GENERIERUNG      :  21.08.2018/10:54:25
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00078!11.09.14!2.1  ! NEUERSTELLUNG             ! VON HELDEN
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DRMA3475         ! WARZ
      * AMA00400!06.08.18!2.3  ! ÄNDERUNG DRMA3475         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5048.
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
              07 DRMA3475.
                 09 FILE-KEY.
                    11 VBNR             pic 9(9).
                    11 PG               pic 99.
                    11 VERTRAGS-ID      pic X(15).
                    11 STRUK-BEGRIFF    pic X(10).
                    11 PRODSCHL         pic 999.
                    11 REIHENFOLGE      pic X(26).
                 09 BEGINN              pic X(7).
                 09 ABLAUF              pic X(7).
                 09 PROVAUS             pic S9(9)V99   packed-decimal.
                 09 PROVEINB            pic S9(9)V99   packed-decimal.
                 09 PROVBRUTTO          pic S9(9)V99   packed-decimal.
                 09 GEVO-AKT            pic 9(5).
                 09 HAFTZEIT-KZN        pic XX.
                 09 HZRBEG-VB           pic X(10).
                 09 HZREND-VB           pic X(10).
                 09 HZRBEG-PG           pic X(10).
                 09 HZREND-PG           pic X(10).
                 09 VN                  pic X(20).
                 09 VERF-ALT-NEU-KZN    pic X.
                 09 NVP                 pic S9(9)V99   packed-decimal.
                 09 BUCHDAT             pic X(10).
                 09 PRODART             pic 99.
                 09 GEL-KENNUNG         pic X(6).
      *
      **************************************************************

