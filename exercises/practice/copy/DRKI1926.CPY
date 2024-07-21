      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1926.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1926.CPv  $
      *
      *   Rev 2.1   Jun 28 2006 15:52:54   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en  OZPs
      *
      *   Rev 2.0   Jun 22 2006 12:17:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1926                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1926                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.06.2006/15:17:25
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
      *    FÜR DIE FUNKTION DRKI1926
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1926.
           05  PI-DRKI1926-KEY.


             10 PI-DRKI1926-DBUGHPLFN   PIC S9(4) COMP.
             10 PI-DRKI1926-DBUJAHR   PIC S9(4) COMP.
             10 PI-DRKI1926-DBULFDNR   PIC S9(4) COMP.
             10 PI-DRKI1926-DBUMONAT   PIC S9(4) COMP.
             10 PI-DRKI1926-DBUVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1926-DATEN.


             10 PI-DRKI1926-DBUHABEN   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI1926-DBUSOLL   PIC S9(12)V9(2) COMP-3.
             
      *
       01  PO-DRKI1926.
           05  PO-DRKI1926-RC                 PIC  X(003).
               88 PO-DRKI1926-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1926-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
