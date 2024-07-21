      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0114.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0114.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   08 Aug 1998 13:13:56   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   Mar 06 1997 14:59:44   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0114  FUNKTION                           *
      *                           "Gesamtprovision berechnen"        *
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
      *         !        !    !                           !          *
011097* AFPA0012!01.10.97! 02 ! NEUE FACHFUNKTION         ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0012.
           05 PI-UPPA0012-FUNKTION            PIC X(008).
              88 PI-UPPA0012-DRPA0125             VALUE 'DRPA0125'.
011097        88 PI-UPPA0012-DRPA0539             VALUE 'DRPA0539'.
