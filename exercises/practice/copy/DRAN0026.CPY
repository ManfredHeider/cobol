      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0026.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0026.CPv  $
      *
      *   Rev 2.2   Mar 18 2005 10:35:18   t13ae34
      *AKI00479: Funktion: eingereichte Einheiten zu VB in best. Ze
      *itraum
      *
      *   Rev 2.1   11 Sep 2000 12:25:26   T13AE34
      *AAN00279-konzessionspflichtiges Geschäft
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0026 FUNKTIONSAUFRUFE FÜR ZAUU0010      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    08.09.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00279!08.09.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AKI00279!16.03.05!2.3 ! DRAN0108                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZAUU0010.
           05  PI-ZAUU0010-FUNKTION            PIC  X(008).
               88 PI-ZAUU0010-DRAN0027-LESEN        VALUE 'DRAN0027'.
080900*        CICS-VERSION NICHT IMPLEMENTIERT
080900*        88 PI-ZDUU0010-DRAN0027-LESEN        VALUE 'DRAN0027'.
               88 PI-ZBUU0010-DRAN0027-LESEN        VALUE 'DRAN0027'.
               88 PI-ZAUU0010-DRAN0108-LESEN        VALUE 'DRAN0108'.
160305*        CICS-VERSION NICHT IMPLEMENTIERT
160305*        88 PI-ZDUU0010-DRAN0108-LESEN        VALUE 'DRAN0108'.
               88 PI-ZBUU0010-DRAN0108-LESEN        VALUE 'DRAN0108'.
