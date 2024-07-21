      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0073.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0073.CPv  $
      *
      *   Rev 2.3   May 29 2018 11:34:28   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife. Fehlende 
      *copys.
      *
      *   Rev 2.2   Mar 13 2018 17:29:00   T13AE34
      *AAN01351: M_9000_2b - Löschung der Antragsdaten nach Widersp
      *ruch
      *
      *   Rev 2.1   08 Sep 2006 09:40:22   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   Mar 18 2002 09:15:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0073  FUNKTIONEN STANDARD-OZP            *
      *                           PZMB TBAN0007                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00685!08.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01351!13.03.18!2.2 ! DRAN0331                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0007.
           03 PI-ZTAN0007-FUNKTION            PIC X(008).
              88 PI-ZTAN0007-DRAN0112-INS         VALUE 'DRAN0112'.
              88 PI-ZTAN0007-DRAN0105-UPD         VALUE 'DRAN0105'.
              88 PI-ZTAN0007-DRAN0115-DEL         VALUE 'DRAN0115'.
              88 PI-ZTAN0007-DRAN0116-SEL-VN      VALUE 'DRAN0116'.
              88 PI-ZTAN0007-DRAN0117-SEL-VN-AV   VALUE 'DRAN0117'.
              88 PI-ZTAN0007-DRAN0118-SEL-PIN     VALUE 'DRAN0118'.
              88 PI-ZTAN0007-DRAN0119-SEL-PIN-A   VALUE 'DRAN0119'.
              88 PI-ZTAN0007-DRAN0331-DEL-VN      VALUE 'DRAN0331'.


