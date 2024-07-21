      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0290.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0290.CPv  $
      *
      *   Rev 2.7   Mar 13 2018 08:24:28   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.6   Feb 25 2016 10:12:58   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 12 2016 07:57:50   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 11 2016 16:48:54   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Jan 22 2016 09:17:42   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.2   Jan 22 2016 09:10:32   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.1   Aug 26 2015 12:05:58   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 10 2015 13:00:16   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0280  FUNKTIONEN STANDARD-OZP            *
      *                           CKV TBAN0023                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!26.08.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01351!12.03.18!2.2 ! DRAN0327                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0023.
           03 PI-ZTAN0023-FUNKTION            PIC X(008).
              88 PI-ZTAN0023-DRAN0291-INS         VALUE 'DRAN0291'.
              88 PI-ZTAN0023-DRAN0292-SEL-VN      VALUE 'DRAN0292'.
              88 PI-ZTAN0023-DRAN0293-SEL-PIN     VALUE 'DRAN0293'.
              88 PI-ZTAN0023-DRAN0327-DEL-VN      VALUE 'DRAN0327'.
      *
      ****** ENDE COPY-MEMBER ****************************************

