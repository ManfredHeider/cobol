      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0120.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0120.CPv  $
      *
      *   Rev 2.6   Apr 20 2018 14:44:34   t13ae34
      *FAN01127: Erstellzeitpunkt wird in TBAN0008 verändert.
      *
      *   Rev 2.5   Mar 10 2010 09:26:30   t13ae34
      *AAN00944: OPC-Jobs für Anträge in den Testumgebungen
      *
      *   Rev 2.4   Jul 24 2007 09:20:40   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.3   12 Feb 2007 11:40:40   t13ex44
      *AAN00655: Erweiterung des PS auf 3 Stellen - MSC
      *
      *   Rev 2.2   Sep 26 2006 09:56:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2006 16:21:42   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:25:30   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0120  FUNKTIONEN STANDARD-OZP            *
      *                           ANTA TBAN0008                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!12.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00655!22.09.06!2.2 ! ÄNDERUNG NAMEN            ! HEIDER   *
      * AAN00655!05.02.07!2.3 ! NEUE FUNKTION: DRAN0187   ! SCHEIDEL *
      * AAN00764!20.07.07!2.4 ! NEUE FUNKTION: DRAN0191   ! SCHEIDEL *
      * AAN00944!16.02.10!2.5 ! NEUE FUNKTION: DRAN0256   ! HEIDER   *
      * FAN01127!07.03.18!2.6 ! NEUE FUNKTION: DRAN0323   ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0008.
           03 PI-ZTAN0008-FUNKTION            PIC X(008).
              88 PI-ZTAN0008-DRAN0121-INS         VALUE 'DRAN0121'.
              88 PI-ZTAN0008-DRAN0122-UPD         VALUE 'DRAN0122'.
              88 PI-ZTAN0008-DRAN0123-DEL         VALUE 'DRAN0123'.
              88 PI-ZTAN0008-DRAN0124-SEL         VALUE 'DRAN0124'.
              88 PI-ZTAN0008-DRAN0125-SEL-VORS    VALUE 'DRAN0125'.
050207        88 PI-ZTAN0008-DRAN0187-SEL-PIN     VALUE 'DRAN0187'.
200707        88 PI-ZTAN0008-DRAN0191-UPD-ANST    VALUE 'DRAN0191'.
160210        88 PI-ZTAN0008-DRAN0256-SEL-MAX     VALUE 'DRAN0256'.
070318        88 PI-ZTAN0008-DRAN0323-INS-ERSTZ   VALUE 'DRAN0323'.


