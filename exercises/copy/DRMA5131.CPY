      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5131.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5131.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:30:44   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Mar 15 2016 20:42:36   t13ac19
      *AMA00227: 360 Grad - GRAD-13 (JIRA) - Copy-Strecken und Prog
      *ramme
      *
      *   Rev 2.0   Jan 25 2016 17:06:38   t13ac19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: 360 GRAD PROVISIONEN
      *
      *     SOURCE-NAME      :  DRMA5131
      *
      *     GENERIERUNG      :  02.10.2017/14:14:21
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00227!25.01.16!2.1  ! NEUERSTELLUNG             ! O.WILHELM
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5131.
              07 BUCHDAT                pic X(10).
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 LEBEN-KZN              pic XX.
              07 ANTEIL-TYP-UST         pic X.
              07 ERWERB-FOLGE-KZN       pic X.
              07 PROV-AUS               pic S9(11)V99  packed-decimal.
              07 PROV-EINB              pic S9(11)V99  packed-decimal.
              07 PROV-VTF               pic S9(11)V99  packed-decimal.
              07 PROV-VWF               pic S9(11)V99  packed-decimal.
              07 PROV-GES               pic S9(11)V99  packed-decimal.
      *
      **************************************************************

