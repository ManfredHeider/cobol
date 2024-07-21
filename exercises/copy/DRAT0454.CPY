      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0454.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0454.CPv  $
      *
      *   Rev 2.2   Aug 14 2009 09:01:22   t13ex14
      *AKI00779: Änderung DTA- u. Ablaufkontrolle bzgl. Lfdnr.
      *
      *   Rev 2.0   Jan 26 2009 14:56:52   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0454                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0454                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.01.2009/15:25:14
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
      *    FÜR DIE FUNKTION DRAT0454
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0454.
           05  PI-DRAT0454-KEY.


             10 PI-DRAT0454-ABKJOBNAME   PIC X(8).
             10 PI-DRAT0454-ABKLFDNR   PIC S9(9) USAGE COMP.
             10 PI-DRAT0454-ABKMANDANTNR   PIC S9(9) USAGE COMP.
             10 PI-DRAT0454-ABKPARTGESID   PIC S9(4) USAGE COMP.
             

           05  PI-DRAT0454-DATEN.


             10 PI-DRAT0454-ABKBEGABLAUF   PIC X(26).
             10 PI-DRAT0454-ABKBEGAKTU   PIC X(26).
             10 PI-DRAT0454-ABKBEGEINARB   PIC X(26).
             10 PI-DRAT0454-ABKENDAKTU   PIC X(26).
             10 PI-DRAT0454-ABKENDEINARB   PIC X(26).
             10 PI-DRAT0454-ABKKATALOGDATUM   PIC X(10).
             10 PI-DRAT0454-ABKPARTNERDATUM   PIC X(10).
             
      *
       01  PO-DRAT0454.
           05  PO-DRAT0454-RC                 PIC  X(003).
               88 PO-DRAT0454-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0454-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
