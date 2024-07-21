      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0595.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0595.CPv  $
      *
      *   Rev 2.1   23 Aug 2007 09:52:54   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:17:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   22 Oct 1997 13:13:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0595                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0595                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    07.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.11.97! 01 ! NEU                       ! D.WILHELM*
      * APA00639!22.06.07!2.1 ! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0595
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0595 DES BUFFER-OZP ZYPA0015 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PG20, Liste hist. Produktgeberdaten
      *
      ****************************************************************
      *
       01  PI-DRPA0595.
           05  PI-DRPA0595-UFKT                PIC  9(02).
           05  PI-DRPA0595-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0595-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0595-PMPG-ID             PIC  S9(15) COMP-3.
      *
       01  PO-DRPA0595.
           05  PO-DRPA0595-RC                  PIC  X(03).
               88 PO-DRPA0595-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0595-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0595-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0595-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA0595-ERGEBNISZEILE.
               10 PO-DRPA0595-PMPG-VERSION        PIC  9(04).
               10 PO-DRPA0595-PMPG-VNR            PIC  X(15).
220607*        10 PO-DRPA0595-PMPG-PIN            PIC  9(07).
220607         10 PO-DRPA0595-PMPG-PIN            PIC  9(09).
               10 PO-DRPA0595-PDTA-ID             PIC  9(15).
      *
      ******* ENDE COPY-MEMBER **************************************
