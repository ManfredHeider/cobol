      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1560.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRPA1560.CPv  $
      *
      *   Rev 2.1   May 20 2003 12:15:08   t13ae34
      *AAN00443 - autom. Pflegen Vorgabe Strukturerfassungsdatum 
      *
      *   Rev 2.0   Jul 04 2002 15:29:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1560                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1560                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    20.05.2003                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00443!20.05.03!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1560
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    ERMITTELN WOCHENTAG, TAG IM MONAT ZU DATUM
      ****************************************************************
      *
       01  PI-DRPA1560.
           05  PO-DRPA1560-DATEN.
               10 PI-DRPA1560-DATUM-EUR       PIC  X(010).
      *
       01  PO-DRPA1560.
           05  PO-DRPA1560-RC                 PIC  X(003).
               88 PO-DRPA1560-RC-OK                        VALUE 'OK '.
      *
           05  PO-DRPA1560-DATEN.
               10 PO-DRPA1560-WOCHENTAG       PIC  9(001).
                  88 PO-DRPA1560-MONTAG       VALUE 2.
                  88 PO-DRPA1560-DIENSTAG     VALUE 3.
                  88 PO-DRPA1560-MITTWOCH     VALUE 4.
                  88 PO-DRPA1560-DONNERSTAG   VALUE 5.
                  88 PO-DRPA1560-FREITAG      VALUE 6.
                  88 PO-DRPA1560-SAMSTAG      VALUE 7.
                  88 PO-DRPA1560-SONNTAG      VALUE 1.
               10 PO-DRPA1560-TAG-IM-MONAT    PIC S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
