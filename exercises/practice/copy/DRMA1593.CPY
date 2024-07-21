      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1593.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1593.CPv  $
      *
      *   Rev 2.1   Mar 12 2014 13:43:12   t13ac20
      *AMA00044: PDF-Generierung: übernommene Member aus Aachen
      *
      *   Rev 2.0   Nov 26 2013 16:09:30   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: PDF NOOP-ZEILE
      *
      *     SOURCE-NAME      :  DRMA1593
      *
      *     GENERIERUNG      :  12.03.2014/08:53:50
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00044!11.03.14!2.1  ! NEU WEGEN PDF-ERSTELLUNG  !VON HELDEN
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1593.
              07 VORSCHUB               pic X.
              07 AFPDS-SFI.
                 09 AFPDS-LENGTH        pic S9(4)      binary.
                 09 AFPDS-ID            pic XXX.
                 09 AFPDS-FLAG-BYTE     pic X.
                 09 AFPDS-SEQNUM        pic XX.
              07 PDFPFAD                pic X(72).
      *
      **************************************************************
