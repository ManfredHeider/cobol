      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1053.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1053.CPv  $
      *
      *   Rev 2.1   May 27 1999 12:35:08   T13EX26
      *NT-Umstellung
      *
      *   Rev 2.0   May 26 1999 16:58:18   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 14:01:10   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1053  FUNKTION DRPA1053 SELECT PDPT/PDPE *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    25.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!25.08.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1053.
          05 PI-DRPA1053-PROGRAMM              PIC  X(008).
          05 PI-DRPA1053-PDPT-ID            PIC S9(15)V USAGE COMP-3.
      *
       01 PO-DRPA1053.
          05 PO-DRPA1053-RC                         PIC  X(003).
             88 PO-DRPA1053-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1053-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1053-PDPT-PDPE.
      *
          COPY DRPA0451.
      *
      ******* ENDE COPY-MEMBER **************************************
