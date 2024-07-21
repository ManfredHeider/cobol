      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0626.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0626.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:18:14   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:56:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0626                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0626                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Wilhelm                                        *
      *    DATUM:    13.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !13.11.97!    ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0626
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0626.
           05  PI-DRPA0626-EINGABE.
               10 PI-DRPA0626-DTA-ID           PIC S9(15) USAGE COMP-3.
       01  PO-DRPA0626.
           05  PO-DRPA0626-RC                  PIC  X(003).
               88 PO-DRPA0626-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0626-RC-NOT-OK                    VALUE 'NOK'.
               88 PO-DRPA0626-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRPA0626-ERGEBNIS.
               10 PO-DRPA0626-ANZAHL-VSTAT-S   PIC  S9(009) USAGE COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
