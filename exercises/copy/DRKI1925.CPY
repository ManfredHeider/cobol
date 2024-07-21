      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1925.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1925.CPv  $
      *
      *   Rev 2.1   Jun 28 2006 15:52:44   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en  OZPs
      *
      *   Rev 2.0   Jun 22 2006 12:17:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1925                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1925                    *
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
      *    FÜR DIE FUNKTION DRKI1925
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1925.
           05  PI-DRKI1925-KEY.


             10 PI-DRKI1925-DBUGHPLFN   PIC S9(4) COMP.
             10 PI-DRKI1925-DBUJAHR   PIC S9(4) COMP.
             10 PI-DRKI1925-DBULFDNR   PIC S9(4) COMP.
             10 PI-DRKI1925-DBUMONAT   PIC S9(4) COMP.
             10 PI-DRKI1925-DBUVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1925.
           05  PO-DRKI1925-RC                 PIC  X(003).
               88 PO-DRKI1925-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1925-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1925-DATEN.


             10 PO-DRKI1925-DBUHABEN   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI1925-DBUSOLL   PIC S9(12)V9(2) COMP-3.
             

      *
      ******* ENDE COPY-MEMBER **************************************
