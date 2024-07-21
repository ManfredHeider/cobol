      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0151.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0151.CPv  $
      *
      *   Rev 2.4   Jun 02 2015 15:01:20   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher 
      *Status für Daisy
      *
      *   Rev 2.3   10 Dec 2007 15:17:04   t13ae30
      *AVB00905: Allfinanz Stufe 1 im VB-System
      *
      *   Rev 2.2   30 Oct 2007 11:07:36   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.1   Sep 02 1999 12:09:48   T13AE30
      *
      *
      *   Rev 2.0   Mar 09 1999 14:30:56   T13AE35
      *AAS00096 - neue Fkt
      *
      *   Rev 1.1   12 Aug 1998 16:55:04   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   Feb 19 1997 09:58:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0151  FUNKTION                           *
      *                           "Lese-OZP zu KL VIVBA040 "         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.08.1996                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !08.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      * AKI00001!13.07.98! 02 ! Fkt Neu DRVB0259          !Zgur-Vujic*
      * AAS00096!19.02.99! 03 ! NEUE FUNKTION DRVB0359    ! WALB     *
      * AAS00051!23.08.99! 04 ! NEUE FUNKTION DRVB0440    ! WILLIG   *
      * AVB00905!21.06.07! 05 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      * AVB01347!26.05.15! 06 ! NEUE FUNKTION DRVB1103    !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0040.
           05 PI-ZWVB0040-FUNKTION            PIC X(008).
              88 PI-ZWVB0040-DRVB0152             VALUE 'DRVB0152'.
              88 PI-ZWVB0040-DRVB0259             VALUE 'DRVB0259'.
190299        88 PI-ZWVB0040-DRVB0359             VALUE 'DRVB0359'.
230899        88 PI-ZWVB0040-DRVB0440             VALUE 'DRVB0440'.
210607        88 PI-ZWVB0040-DRVB0856             VALUE 'DRVB0856'.
210607        88 PI-ZWVB0040-DRVB0861             VALUE 'DRVB0861'.
210607        88 PI-ZWVB0040-DRVB0881             VALUE 'DRVB0881'.
260515        88 PI-ZWVB0040-DRVB1103             VALUE 'DRVB1103'.

