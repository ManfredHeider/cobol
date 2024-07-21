      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0280.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0280.CPv  $
      *
      *   Rev 2.4   Mar 12 2018 14:50:54   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.3   Feb 25 2016 10:11:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:56:32   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:04:44   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 11:15:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0280  FUNKTIONEN STANDARD-OZP            *
      *                           AGI TBAN0021                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01351!12.03.18!2.2 ! DRAN0326                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0021.
           03 PI-ZTAN0021-FUNKTION            PIC X(008).
              88 PI-ZTAN0021-DRAN0281-INS         VALUE 'DRAN0281'.
              88 PI-ZTAN0021-DRAN0282-SEL-VN      VALUE 'DRAN0282'.
              88 PI-ZTAN0021-DRAN0283-SEL-PIN     VALUE 'DRAN0283'.
120318        88 PI-ZTAN0021-DRAN0326-DEL-VN      VALUE 'DRAN0326'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 



