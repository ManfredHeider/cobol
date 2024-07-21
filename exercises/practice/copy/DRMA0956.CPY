      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0956.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRMA0956.CPv  $
      *
      *   Rev 2.6   Jun 08 2020 14:23:36   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.5   Oct 19 2017 14:12:00   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   Jul 04 2016 08:54:12   t13ac17
      *AMA00266: USt: LUCY II - KOMA0507-Statistiken - Copies
      *
      *   Rev 2.3   Oct 01 2015 12:56:28   t13ac19
      *AMA00199: USt: KOMA0505-VB-Abrechnung - Teil 2 - Copies
      *
      *   Rev 2.2   Sep 01 2015 18:02:46   t13ac19
      *AMA00199: USt: KOMA0505-VB-Abrechnung - Teil 2 - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:09:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Daten zur Erst. Sammelliste der Überweisungen
      *
      *     SOURCE-NAME      :  DRMA0956
      *
      *     GENERIERUNG      :  19.05.2020/12:45:46
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00011!28.06.13!2.1  ! KALYPSO                   !
      * AMA00199!01.09.15!2.2  ! AUFBAU-BONUS DURCH        ! O.WILHELM
      *         !        !     ! PROVAUS-UST ERSETZT       !
      * AMA00199!01.10.15!2.3  ! PROVAUS-UST DURCH         ! O.WILHELM
      *         !        !     ! PROV-UST ERSETZT          !
      * AMA00217!16.02.16!OHNE ! VARIABLENNAME ERSTELLUNG: ! O.WILHELM
      *         !        !     ! DEMA0180 => DEMA0042      !
      *         !        !     ! (KEINE AUSWIRKUNG AUF GE- !
      *         !        !     !  NERIERUNG D. COPYSTRECKE)!
      * AMA00266!15.03.16!2.4  ! neu: ERFOLGS-PROV-UST     ! Walbert
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG EINBEHALT        ! WARZ
      * AMA00476!05.05.20!2.6  ! Erweitern BKTO-INH auf    ! D.Wilhelm
      *    MA577!        !     ! 40 Stellen                !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0956.
              07 STRUK-DIR              pic 999.
              07 VBNR                   pic X(10).
              07 NAME                   pic X(20).
              07 ABRECH-NETTO           pic S9(9)V99   packed-decimal.
              07 ERFOLGS-PROV           pic S9(9)V99   packed-decimal.
              07 PROV-UST               pic S9(9)V99   packed-decimal.
              07 ERFOLGS-PROV-UST       pic S9(9)V99   packed-decimal.
              07 SALDO-VORTR            pic S9(9)V99   packed-decimal.
              07 UEBERW                 pic S9(9)V99   packed-decimal.
              07 SALDO-NEU              pic S9(9)V99   packed-decimal.
              07 BUCHUNGEN              pic S9(9)V99   packed-decimal.
              07 ERSTELLUNG             pic X(40).
              07 STERN                  pic X.
              07 EINB-AUFL              pic S9(9)V99   packed-decimal.
              07 PRAXISSTUFE            pic S9(4)      binary.
              07 BKTO-INH               pic X(40).
              07 IBAN                   pic X(34).
              07 FILLER                 pic X(7).
      *
      **************************************************************
