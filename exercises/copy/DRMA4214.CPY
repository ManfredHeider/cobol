      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4214.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA4214.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:21:32   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 17:21:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: normierte Prov.-Bew. KD-Betr.-Prov.
      *
      *     SOURCE-NAME      :  DRMA4214
      *
      *     GENERIERUNG      :  02.10.2017/13:28:29
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG DGMA4194         ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4214.
              07 BUCHDAT-VG             pic X(10).
              07 VBNR                   pic 9(9).
              07 PG                     pic 99.
              07 VERTRAGS-ID-VBI        pic X(15).
              07 DGOV4194.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 PRAXIS-STUFE        pic 9.
                 09 VERTRAGS-ID         pic X(15).
                 09 FAELLIG             pic X(10).
                 09 PRODSCHL            pic 999.
                 09 PRODART             pic 99.
                 09 PE-VGSATZ           pic S999V99    packed-decimal.
                 09 PE-PROVISION        pic S9(7)V99   packed-decimal.
                 09 PE-VGSATZ-KDBETR    pic S999V99    packed-decimal.
                 09 PE-PROVISION-KDBETR pic S9(7)V99   packed-decimal.
      *
      **************************************************************

