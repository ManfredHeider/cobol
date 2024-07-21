      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1892.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1892.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 12:50:24   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   May 15 2006 09:12:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1892                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1892                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.05.2006/12:08:27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1892
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1892.
           05  PI-DRKI1892-KEY.


             10 PI-DRKI1892-NKDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1892-NKDPERSONENID   PIC X(13).
             

           05  PI-DRKI1892-DATEN.


             10 PI-DRKI1892-NKDBWTUNGSKZDVAG   PIC X(2).
             10 PI-DRKI1892-NKDBWTUNGSKZVB   PIC X(2).
             10 PI-DRKI1892-NKDDIREKTION   PIC S9(4) COMP.
             10 PI-DRKI1892-NKDEREIGNISKZ   PIC S9(4) COMP.
             10 PI-DRKI1892-NKDKNDSTATUSIDALT   PIC X(1).
             10 PI-DRKI1892-NKDKNDSTATUSIDNEU   PIC X(1).
             10 PI-DRKI1892-NKDKOMMISARISCHKZ   PIC X(1).
             10 PI-DRKI1892-NKDKUNDENNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1892-NKDKUZUPERSONENID   PIC X(13).
             10 PI-DRKI1892-NKDPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1892-NKDPGMNAME   PIC X(8).
             10 PI-DRKI1892-NKDPRODUKT   PIC X(10).
             10 PI-DRKI1892-NKDTAGESDATUM   PIC X(10).
             10 PI-DRKI1892-NKDVBNR   PIC S9(9) COMP.
             10 PI-DRKI1892-NKDVERTRAGSNUMMER   PIC X(15).
             
      *
       01  PO-DRKI1892.
           05  PO-DRKI1892-RC                 PIC  X(003).
               88 PO-DRKI1892-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1892-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
