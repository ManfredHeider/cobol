      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0555.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0555.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:15:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Oct 1997 11:31:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0555                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0555                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Wilhelm                                        *
      *    DATUM:    20.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.10.97!    ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0555
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0555.
           05  PI-DRPA0555-EINGABE.
               10 PI-DRPA0555-DTA-ID           PIC S9(15) USAGE COMP-3.
       01  PO-DRPA0555.
           05  PO-DRPA0555-RC                  PIC  X(003).
               88 PO-DRPA0555-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0555-RC-NOT-OK                    VALUE 'NOK'.
               88 PO-DRPA0555-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRPA0555-ERGEBNIS.
               10 PO-DRPA0555-ANZAHL-FEHLER    PIC  S9(009) USAGE COMP.
               10 PO-DRPA0555-ANZAHL-BEWEGUNG  PIC  S9(009) USAGE COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
