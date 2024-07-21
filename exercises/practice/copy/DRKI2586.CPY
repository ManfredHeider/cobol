      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2586.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2586.CPv  $
      *
      *   Rev 2.2   Dec 17 2012 11:13:36   t13ae49
      *FKI01384: Wartungsauftrag NKDA - VN-IDENT wg. vertrauschte K
      *TO-I anpassen
      *
      *   Rev 2.1   Sep 20 2012 10:47:54   t13ae49
      *AKI00886: Host - neues Design Kundenaktualisierung - Hauptpr
      *ogramm
      *
      *   Rev 2.0   Mar 29 2012 14:58:34   t13ae49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2586                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2586                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!29.03.12! 2.0! Neu                       ! Wehlitz  *
      * --------+--------+----+---------------------------+----------*
      * FKI01384!13.12.12! 2.1! Tabellenerweiterung um    ! Wehlitz  *
      *         !        !    ! 3 Pruefwerte              !          *
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI2586
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT NACH PRUEFWERT UND SATZANZAHL <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2586.
           05  PI-DRKI2586-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI2586-KDKPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2586-KDKPRUEFWERT   PIC S9(9) COMP.
131212       10 PI-DRKI2586-KDKPRUEFWERT2  PIC S9(9) COMP.
131212       10 PI-DRKI2586-KDKPRUEFWERT3  PIC S9(9) COMP.
131212       10 PI-DRKI2586-KDKPRUEFWERT4  PIC S9(9) COMP.
             10 PI-DRKI2586-KDKSATZANZAHL   PIC S9(9) COMP.
END   *

      *
       01  PO-DRKI2586.
           05  PO-DRKI2586-RC                     PIC  X(003).
               88 PO-DRKI2586-RC-OK               VALUE 'OK '.
               88 PO-DRKI2586-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2586-DATEN.
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2586-KDKJOBNAME   PIC  X(0008).
             10 PO-DRKI2586-KDKLAUFTERMIN   PIC  X(026).
END   *
      *
      ******* ENDE COPY-MEMBER ***************************************
