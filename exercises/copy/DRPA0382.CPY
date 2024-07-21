      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0382.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0382.CPv  $
      *
      *   Rev 2.2   Aug 13 2015 15:32:22   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11, PO25)
      *
      *   Rev 2.1   Jul 16 2015 15:10:46   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.0   Feb 27 1999 14:28:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:27:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:29:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0382                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0382                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                        *
      *    DATUM:    24.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.07.97! 01 ! NEU                       ! SCHRIEBER*
      * APA01932!14.07.15! 2.1! PO-DRPA0382-UST-BETRAG NEU! WANG     *
      *         !        ! 2.2! PO ÄNDERUNGEN             ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0382
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0382 DES BUFFER-OZP ZYPA0002 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PA15, ANTRäGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA0382.
           05  PI-DRPA0382-UFKT                PIC  9(02).
           05  PI-DRPA0382-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0382-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0382-DATEN.
               10 PI-DRPA0382-DTA-ID           PIC S9(15) COMP-3.
               10 PI-DRPA0382-VSTAT-KLASSE     PIC  X(01).
      *
       01  PO-DRPA0382.
           05  PO-DRPA0382-RC                  PIC  X(03).
               88 PO-DRPA0382-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0382-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0382-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0382-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA0382-ERGEBNISZEILE.
               10 PO-DRPA0382-VSTAT               PIC  X(04).
               10 PO-DRPA0382-ANZAHL-BEWEGUNG     PIC  9(09) USAGE COMP.
               10 PO-DRPA0382-SUMME-GUTSCHRIFTEN  PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
               10 PO-DRPA0382-SUMME-BELASTUNGEN   PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
               10 PO-DRPA0382-PG-GSMT-PROV-OZ     PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
               10 PO-DRPA0382-DIFFERENZ           PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
100815*        10 PO-DRPA0382-UST-BETRAG          PIC  S9(09)V9(02)
100815*                                                PACKED-DECIMAL.
100815         10 PO-DRPA0382-SUMME-UST-GUT       PIC  S9(09)V9(02)
                                                       PACKED-DECIMAL.
100815         10 PO-DRPA0382-SUMME-UST-BEL       PIC  S9(09)V9(02)
100915                                                 PACKED-DECIMAL.
100815         10 PO-DRPA0382-UST-PROV-OZ         PIC  S9(09)V9(02)
100915                                                 PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


