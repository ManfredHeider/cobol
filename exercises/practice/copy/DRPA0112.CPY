      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0112.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0112.CPv  $
      *
      *   Rev 2.0   Mar 05 1999 10:31:48   T13EX21
      *Erweiterung 
      *
      *   Rev 1.1   Mar 06 1997 14:59:24   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0112  FUNKTIONEN "PROV.-STRUK. PRUEFEN"  *
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
      * DAISY 1 !28.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      * APA0012 !21.09.98! 01 ! NEUE FUNKTIONEN           ! STORCH   *
      *         !        !    ! - DRPA1073                !          *
      *         !        !    ! - DRPA1074                !          *
      * APA0012 !04.03.99! 01 ! NEUE FUNKTION             ! STORCH   *
      *         !        !    ! - DRPA0556                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0005.
           05 PI-UWPA0005-FUNKTION            PIC X(008).
              88 PI-UWPA0005-DRPA0123             VALUE 'DRPA0123'.
210998        88 PI-UWPA0005-DRPA1073             VALUE 'DRPA1073'.
210998        88 PI-UWPA0005-DRPA1074             VALUE 'DRPA1074'.
040399        88 PI-UWPA0005-DRPA0556             VALUE 'DRPA0556'.
