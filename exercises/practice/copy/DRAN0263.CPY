      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0263.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0263.CPv  $
      *
      *   Rev 2.5   Mar 13 2018 09:12:38   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.4   Jun 22 2015 15:44:50   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 17:38:56   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:48:04   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:23:56   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:46:10   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0263  FUNKTIONEN STANDARD-OZP            *
      *                           DVM  TBAN00020                     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01351!12.03.18!2.5 ! DRAN0330                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0020.
           03 PI-ZTAN0020-FUNKTION            PIC X(008).
              88 PI-ZTAN0020-DRAN0264-INS         VALUE 'DRAN0264'.
              88 PI-ZTAN0020-DRAN0265-UPD         VALUE 'DRAN0265'.
              88 PI-ZTAN0020-DRAN0266-DEL         VALUE 'DRAN0266'.
              88 PI-ZTAN0020-DRAN0267-SEL-VN      VALUE 'DRAN0267'.
              88 PI-ZTAN0020-DRAN0268-SEL-VN-A    VALUE 'DRAN0268'.
              88 PI-ZTAN0020-DRAN0269-SEL-PIN     VALUE 'DRAN0269'.
              88 PI-ZTAN0020-DRAN0270-SEL-PIN-A   VALUE 'DRAN0270'.
120318        88 PI-ZTAN0020-DRAN0330-DEL-VN      VALUE 'DRAN0330'.
      *
      ****** ENDE COPY-MEMBER ****************************************

