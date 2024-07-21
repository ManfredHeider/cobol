      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1861.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1861.CPv  $
      *
      *   Rev 2.1   Mar 12 2014 13:43:30   t13ac20
      *AMA00044: PDF-Generierung: übernommene Member aus Aachen
      *
      *   Rev 2.0   Nov 26 2013 16:09:54   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: PDF METADATEN
      *
      *     SOURCE-NAME      :  DRMA1861
      *
      *     GENERIERUNG      :  11.03.2014/18:49:38
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
           05 DRMA1861.
              07 VBNR                   pic 9(7).
              07 SEMI1                  pic X.
              07 LIEFER-PG              pic XX.
              07 SEMI2                  pic X.
              07 POST-TYP-KZN           pic X.
              07 SEMI3                  pic X.
              07 PDF-FACH-KENNUNG       pic X(10).
              07 SEMI4                  pic X.
              07 ERSTELL-DAT            pic 9(8).
              07 SEMI5                  pic X.
              07 ABRECH-MONAT           pic 9(6).
              07 SEMI6                  pic X.
              07 PDFPFAD-VBAB.
                 09 PDFVERZ             pic X(29).
                 09 PDFDATEI            pic X(31).
                 09 FILLER              occurs 40
                                        pic X.
              07 PDFPFAD-RECH redefines PDFPFAD-VBAB.
                 09 PDFVERZ             pic X(26).
                 09 PDFDATEI            pic X(31).
                 09 FILLER              occurs 43
                                        pic X.
              07 SEMI7                  pic X.
              07 DOCSIZE                pic 9(8).
              07 SEMI8                  pic X.
              07 MIME-TYPE              pic X(15).
              07 SEMI9                  pic X.
      *
      **************************************************************
