      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0839.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0839.CPv  $
      *
      *   Rev 2.2   Jul 05 2016 07:56:24   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:07:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für DB2-Table SLM (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0839
      *
      *     GENERIERUNG      :  22.06.2016/08:27:29
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!20.06.16!2.2  ! Erweiterungen zu Lucy2    !   Walbert
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 SLM-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 DAT-VON                pic X(10).
              07 BUCHPERIODE            pic X(6).
              07 HERKUNFT-KZN           pic X.
              07 VERARB-TYP             pic X.
              07 KNR-SLM-TYP            pic S9(4)      binary.
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 LAST-UPDATE.
                 09 FILLER              pic X(10).
                 09 FILLER              pic X(10).
                 09 FILLER              pic X(6).
              07 KTY-SLM-TYP            pic S9(4)      binary.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 AUSZAHLUNG             pic S9(9)V99   packed-decimal.
              07 UST                    pic S9(9)V99   packed-decimal.
      *
      **************************************************************

