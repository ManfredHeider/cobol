      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0066.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0066.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.4   19 Nov 1998 15:28:16   T13EX26
      *apa00033 - Rueckerstellung
      *
      *   Rev 1.3   18 Nov 1998 16:29:58   T13EX26
      *apa00033 - Wiederherstellung
      *
      *   Rev 1.2   13 Nov 1998 09:07:42   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 06 1997 14:54:24   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0066  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      * AFPA0012!23.09.98! 02 ! ANPASSUNG VERSION 3.0     ! LÜTTGES  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0066                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PROT TBPA0010                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0066.
          05 PI-DRPA0066-PROT-KEY.
             10 PI-DRPA0066-PROT-ID          PIC S9(015) PACKED-DECIMAL.
          05 PI-DRPA0066-PROT-DATEN.
             10 PI-DRPA0066-KNR-PROT-ART     PIC S9(004) BINARY.
             10 PI-DRPA0066-PROT-ERSTELL-DAT PIC  X(010).
             10 PI-DRPA0066-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRPA0066-PROT-PTR-ID      PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0066-KNR-PROT-PTR-OBJ PIC S9(004) BINARY.
             10 PI-DRPA0066-KNR-PROT-PTR-AUSPR PIC S9(004) BINARY.
      *
       01 PO-DRPA0066.
          05 PO-DRPA0066-RC                  PIC  X(003).
             88 PO-DRPA0066-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0066-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
