      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0115.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0115.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 10:18:52   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 14:59:48   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0115  FUNKTION                           *
      *                           "Grundprovision berechnen"         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGGEN                                        *
      *    DATUM:    10.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
131097*         !13.10.97!    ! Neue Funktion DRPA0545    ! Jäger    *
131097*         !        !    ! für neue Konditionen einb.!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0013.
           05 PI-UPPA0013-FUNKTION            PIC X(008).
              88 PI-UPPA0013-DRPA0126             VALUE 'DRPA0126'.
011097        88 PI-UPPA0013-DRPA0545             VALUE 'DRPA0545'.
