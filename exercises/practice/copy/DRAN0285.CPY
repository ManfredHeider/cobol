      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0285.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0285.CPv  $
      *
      *   Rev 2.4   Mar 12 2018 15:18:38   t13ae34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.3   Feb 25 2016 10:12:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:57:12   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:05:22   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 09 2015 18:17:38   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0281  FUNKTIONEN STANDARD-OZP            *
      *                           DB  TBAN0022                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN01351!12.03.18!2.2 ! DRAN0328                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0022.
           03 PI-ZTAN0022-FUNKTION            PIC X(008).
              88 PI-ZTAN0022-DRAN0286-INS         VALUE 'DRAN0286'.
              88 PI-ZTAN0022-DRAN0287-SEL-VN      VALUE 'DRAN0287'.
              88 PI-ZTAN0022-DRAN0288-SEL-PIN     VALUE 'DRAN0288'.
120318        88 PI-ZTAN0022-DRAN0328-DEL-VN      VALUE 'DRAN0328'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 



