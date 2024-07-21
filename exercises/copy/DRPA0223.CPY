      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  Januar 1997
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0223.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0223.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:54:12   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:11:36   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0223  FUNKTION STUF-BEZ PRüFEN           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Giese                                           *
      *    DATUM:    10.02.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.02.97! 01 ! Neuerstellung             ! Giese    *
      * DAISY 1 !08.04.97! 01 ! ERWEITERUNG RETURNCODE    ! SCHäFER  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0223                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *   MöGLICHE RETURNCODES DER FUNKTION DRPA0223                 *
      *   ------------------------------------------                 *
      *   ASB  ABWEICHUNG STUFENBEZEICHNUNG - STELLENTYP.            *
      *        DIE MITGEGEBENE STUFENBEZEICHNUNG WEICHT VOM          *
      *        ERMITTELTEN STELLENTYP AB.                            *
      *                                                              *
      *   USB  UNBEKANNTE STUFENBEZEICHNUNG                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0223.
          05 PI-DRPA0223-DATEN.
             10 PI-DRPA0223-ST-NR               PIC S9(09) BINARY.
             10 PI-DRPA0223-STUFEN-BEZ          PIC  X(04).

       01 PO-DRPA0223.
          05 PO-DRPA0223-RC                     PIC  X(003).
             88 PO-DRPA0223-RC-OK                    VALUE 'OK '.
080497       88 PO-DRPA0223-RC-ASB                   VALUE 'ASB'.
080497       88 PO-DRPA0223-RC-USB                   VALUE 'USB'.
          05 PO-DRPA0223-ST-TYP                 PIC S9(04).
      *
      ******** Ende Copy-Member **************************************
