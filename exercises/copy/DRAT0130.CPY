      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0130.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0130.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 12 1997 10:56:34   T13EX11
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:20:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0130                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0130                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *    DATUM:    16.04.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0339!16.04.97! 01 ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0130:
      *            'PROTOKOLLIEREN DIALOG-ZUGRIFFE MIT ST-NR'
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0130.
           05  PI-DRAT0130-DATEN.
               10 PI-DRAT0130-ST-NR            PIC  9(007).
      *
       01  PO-DRAT0130.
           05  PO-DRAT0130-RC                  PIC  X(003).
               88 PO-DRAT0130-RC-PROTOKOLLIERT             VALUE 'PR '.
               88 PO-DRAT0130-RC-N-PROTOKOLLIERT           VALUE 'NPR'.
      *
      ******* ENDE COPY-MEMBER **************************************
