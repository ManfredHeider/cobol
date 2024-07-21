      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0593.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0593.CPv  $
      *
      *   Rev 2.1   23 Aug 2007 09:52:42   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:16:34   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   22 Oct 1997 13:13:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0593                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF LIST ZU PMPG UND PMPGH                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    07.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.11.97!    ! NEU                       ! D.WILHELM*
      * APA00639!27.06.07!2.1 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0593                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0593.
          05 PI-DRPA0593-PROGRAMM            PIC  X(008).
          05 PI-DRPA0593-UFKT                PIC  9(002).
          05 PI-DRPA0593-SCHALTER            PIC  9.
          05 PI-DRPA0593-DATEN.
             10 PI-DRPA0593-PMPG-ID          PIC  9(015).
      *
       01 PO-DRPA0593.
          05 PO-DRPA0593-RC                  PIC  X(003).
             88 PO-DRPA0593-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0593-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0593-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0593-DATEN.
             10 PO-DRPA0593-PMPG-VERSION      PIC 9(04).
             10 PO-DRPA0593-PMPG-VNR          PIC 9(15).
270607*      10 PO-DRPA0593-PMPG-PIN          PIC 9(07).
270607       10 PO-DRPA0593-PMPG-PIN          PIC 9(09).
             10 PO-DRPA0593-PDTA-ID           PIC 9(15).
             10 FILLER                        PIC 9(09).
