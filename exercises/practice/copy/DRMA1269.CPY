      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1269.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1269.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 16:00:02   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:23:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1264 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1269
      *
      *     GENERIERUNG      :  29.06.2015/11:09:28
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * --------!--------!-----!---------------------------!----------
      * AMA00129!29.06.15!2.2  ! Neugen. wg.Update DRMA1107! D.WILHELM
      * --------!--------!-----!---------------------------!----------
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1269.
              07 FUNKTION               pic X(8).
              07 KONTEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 ABR-NR-KTO-H        pic X(15).
                 09 ABR-NR-KTO-D        pic X(15).
                 09 ABR-NR-KTO-A        pic X(15).
                 09 LAST-BUCH-DAT-ABR   pic X(10).
                 09 LAST-BUCH-DAT-PROV  pic X(10).
                 09 NUR-SALDOBUCH-KZN   pic X.
                 09 KTO-STAND-H         pic S9(9)V99   packed-decimal.
                 09 BUCH-DAT-H          pic X(10).
                 09 KTO-STAND-R         pic S9(9)V99   packed-decimal.
                 09 BUCH-DAT-R          pic X(10).
                 09 KTO-STAND-D         pic S9(9)V99   packed-decimal.
                 09 BUCH-DAT-D          pic X(10).
                 09 KTO-STAND-A         pic S9(9)V99   packed-decimal.
                 09 BUCH-DAT-A          pic X(10).
                 09 UEB-BETRAG          pic S9(9)V99   packed-decimal.
                 09 UEB-DATUM           pic X(10).
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************

