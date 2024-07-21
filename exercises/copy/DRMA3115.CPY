      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3115.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA3115.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:15:56   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Jul 28 2015 16:01:56   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:39:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVBUCHDATWERTE00
      *
      *     SOURCE-NAME      :  DRMA3115
      *
      *     GENERIERUNG      :  02.10.2017/12:56:23
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00129!29.06.15!2.2  ! ÄNDERUNG DRMA3114         ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG DRMA3114 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3115.
              07 BUCHDATWERTE.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 IND-JW              pic S9(4)      binary.
                 09 BUCH-DAT            pic X(10).
                 09 DISKONT-BETR-BRUTTO pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-BRUTTO
                                        pic S9(11)V99  packed-decimal.
                 09 DISKONT-BETR-NETTO  pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-NETTO pic S9(11)V99  packed-decimal.
                 09 DISKONT-BETR-UST    pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-UST   pic S9(11)V99  packed-decimal.
                 09 EINB-BETR           pic S9(11)V99  packed-decimal.
                 09 EINB-JBETR          pic S9(11)V99  packed-decimal.
                 09 DARLEHEN-BETR       pic S9(11)V99  packed-decimal.
                 09 DARLEHEN-JBETR      pic S9(11)V99  packed-decimal.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************

