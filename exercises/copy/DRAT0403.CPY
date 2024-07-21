      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0403.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0403.CPv  $
      *
      *   Rev 2.1   Oct 28 2004 09:28:12   t13ex22
      *ASR00001: Elektronische Archivierung - PGM für UDF
      *
      *   Rev 2.0   Oct 28 2004 08:16:16   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0403                                      *
      *    --------------------------------------------------------   *
      *    DSNTIAR-DEKLARATIONEN FÜR USERDEFINED FUNCITIONS (UDF)     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    28.10.2004                                       *
      *****************************************************************
      *    VERSION   1    28.10.04 16:35:20
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASR00001!28.10.04!    ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * ----------------------------------------------------------------
      * DSNTIAR-DEKLARATIONEN                                          -
      * ----------------------------------------------------------------
       01 DSNTIAR.
          03 DSNTIAR-MSG.
             05 FILLER PIC X(32) VALUE
               'FEHLER VON FEHLERFORMATROUTINE: '.
             05 FILLER PIC X(3) VALUE 'RC='.
             05 DSNTIAR-RC PIC -(10).
             05 FILLER PIC X(11) VALUE ', SQLCODE='.
             05 DSNTIAR-SQLCODE PIC -(6).
          03 ERROR-TEXT-LEN     PIC S9(9) COMP     VALUE +80.

          03 ERROR-MESSAGE.
             05 ERROR-LEN PIC S9(4) COMP VALUE +960.
             05 ERROR-TEXT PIC X(80) OCCURS 12 TIMES.

          03 I-ERROR     PIC S9(9) COMP.
          03 AKTUELL-SQL PIC X(72).
