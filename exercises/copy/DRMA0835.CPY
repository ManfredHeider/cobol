      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0835.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA0835.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:11:28   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Aug 27 2015 19:11:28   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:06:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für DB2-Table AST (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0835
      *
      *     GENERIERUNG      :  02.10.2017/12:39:30
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!17.08.15!2.2  ! ANPASSUNGEN AN GEÄNDERTE  ! D.WILHELM
      *         !        !     ! AST WG. LUCY              !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0164 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 AST-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 JEQF-EG                pic S999V99    packed-decimal.
              07 JEQF-GR                pic S999V99    packed-decimal.
              07 JEQF-GS                pic S999V99    packed-decimal.
              07 DGMA0122.
                 09 EH-AB-78-EG         pic S9(9)V9    packed-decimal.
                 09 EH-AB-78-GR         pic S9(9)V9    packed-decimal.
                 09 EH-BIS-78-GS        pic S9(9)V9    packed-decimal.
              07 EH-VM-ZU-EG            pic S9(9)V9    packed-decimal.
              07 EH-VM-ZU-GR            pic S9(9)V9    packed-decimal.
              07 EH-VM-AB-EG            pic S9(9)V9    packed-decimal.
              07 EH-VM-AB-GR            pic S9(9)V9    packed-decimal.
              07 EINB-STZ               pic S999V99    packed-decimal.
              07 KTY-UST-KZ             pic S9(4)      binary.
              07 KNR-UST-KZ             pic S9(4)      binary.
      *
      **************************************************************

