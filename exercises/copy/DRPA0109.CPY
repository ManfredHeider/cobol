      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0109.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0109.CPv  $
      *
      *   Rev 2.1   Jun 22 1999 23:10:52   T13AE18
      *apa00012
      *
      *   Rev 2.0   Feb 27 1999 14:27:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:58:58   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0109  FUNKTIONEN "AVERM/VM ERMITTELN  "  *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRÖMPELER                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !27.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      * APA00012!21.06.99!2.01! NEUE FUBKTION DRPA1268    ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0002.
           05 PI-UWPA0002-FUNKTION            PIC X(008).
              88 PI-UWPA0002-DRPA0120             VALUE 'DRPA0120'.
210699        88 PI-UWPA0002-DRPA1268             VALUE 'DRPA1268'.
