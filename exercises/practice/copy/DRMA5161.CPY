      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5161.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5161.CPv  $
      *
      *   Rev 2.1   Sep 27 2017 14:59:32   t13ac18
      *AMA00362: DVAG-Wien: Sonderleistung ohne Finanzierungseinhei
      *ten 1[AE]
      *
      *   Rev 2.0   Sep 21 2017 11:36:50   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Fortschr. Finanzierungseinh. Wien
      *
      *     SOURCE-NAME      :  DRMA5161
      *
      *     GENERIERUNG      :  21.09.2017/11:50:27
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00362!21.09.17!2.1  ! NEUERSTELLUNG             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5161.
              07 VBNR                   pic X(7).
              07 EKRIT-DATUM            pic 9(6).
              07 DGMA5012               occurs 12
                                        indexed by IND.
                 09 VBKZ                pic X.
                 09 DGMA5013.
                    11 EH-NT-NE-EG-FI   pic S9(9)V9    packed-decimal.
      *
      **************************************************************

