      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:47:16   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0009                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE ZUM UNTERPROGRAMM TIMESTAMP-ERMITTLUNG       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. MUELLER                                       *
      *    DATUM:    16.01.1996                                       *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    ! NEU                       ! MUELLER  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *   - FUNKTION: SCHNITTSTELLE ZUM UNTERPROGRAMM TIMESTAMP-
      *               ERMITTLUNG
      *   - PI-DATEN: - FUNKTIONSAUFRUF
      *   - PO-DATEN: - RETURNCODE: OK ODER NOK
      *               - DATEN: TIMESTAMP PIC X(26)
      *                 9999-99-99-99.99.99.999999
      *                 DATUM PIC X(10)
      *                 9999-99-99
      *
      ****************************************************************

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0009.
           05  PI-DRPA0009-FUNKTION           PIC X(08).
               88  PI-DRPA0009-AUFRUF         VALUE 'DRPA0009'.
           05  PI-DRPA0009-DATEN.
               10  PI-DRPA0009-FILLER         PIC  X(01).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRPA0009.
           05  PO-DRPA0009-RC                 PIC  X(03).
               88  PO-DRPA0009-RC-OK                 VALUE 'OK '.
               88  PO-DRPA0009-RC-NOK                VALUE 'NOK'.
           05  PO-DRPA0009-DATEN.
               10  PO-DRPA0009-TIMESTAMP      PIC  X(26).
               10  PO-DRPA0009-DATUM          PIC  X(10).
      *
      ******** ENDE COPY-MEMBER (DRPA0009) *******************

