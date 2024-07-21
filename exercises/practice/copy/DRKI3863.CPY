      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3863.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3863.CPv  $
      *
      *   Rev 2.1   Feb 28 2021 10:44:36   t13ex39
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT
      *
      *   Rev 2.0   Dec 22 2020 14:41:58   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3863                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3863                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.12.2020 / 16:03:19 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3863
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3863.
           05 PI-DRKI3863-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3863-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3863-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3863-UFKT-QUIT               VALUE 3.
 
           05 PI-DRKI3863-KEY.
              10 PI-DRKI3863-G48MANDANT         PIC  S9(9) COMP.


       01  PO-DRKI3863.
           05  PO-DRKI3863-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PO-DRKI3863-G48SPPLANNR   PIC  X(0002).
             10 PO-DRKI3863-G48SPVNR   PIC  X(0015).
             10 PO-DRKI3863-G48SPWAEHRUNGSKZ   PIC  X(0003).
END   *
 
      *
           05  PO-DRKI3863-RC                     PIC  X(003).
              88 PO-DRKI3863-RC-OK              VALUE 'OK '.
              88 PO-DRKI3863-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3863-RC-EOF             VALUE 'EOF'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3863-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3863-G48SPDEPOTUNTERNR   PIC  X(0002).
             10 PO-DRKI3863-G48SPABWEICHKTOINH   PIC  X(0027).
             10 PO-DRKI3863-G48SPAUSGAKTFOND   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPAUSGFOND   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPBEGINNDYN   PIC  X(0008).
             10 PO-DRKI3863-G48SPBEGRATENAUS   PIC  X(0008).
             10 PO-DRKI3863-G48SPBIC   PIC  X(0011).
             10 PO-DRKI3863-G48SPBONIFIKATION   PIC S9(3)V9(4) COMP-3.
             10 PO-DRKI3863-G48SPDATVERTKUEND   PIC  X(0008).
             10 PO-DRKI3863-G48SPDELKZ   PIC  X(0001).
             10 PO-DRKI3863-G48SPENDERATENAUS   PIC  X(0008).
             10 PO-DRKI3863-G48SPIBAN   PIC  X(0034).
             10 PO-DRKI3863-G48SPISIN   PIC  X(0012).
             10 PO-DRKI3863-G48SPKOSTENPROZENT   PIC S9(1)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPKUENDMERKMAL   PIC  X(0001).
             10 PO-DRKI3863-G48SPMANDAT   PIC  X(0035).
             10 PO-DRKI3863-G48SPOFFNUNGSDATUM   PIC  X(0008).
             10 PO-DRKI3863-G48SPPIN   PIC S9(11)V COMP-3.
             10 PO-DRKI3863-G48SPPROZENTDYN   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPSCHLIESSDATUM   PIC  X(0008).
             10 PO-DRKI3863-G48SPSPARPLENDEVL   PIC  X(0008).
             10 PO-DRKI3863-G48SPSPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPTAGEINZAHLUNG   PIC S9(4) COMP.
             10 PO-DRKI3863-G48SPURGESAMTEURO   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPURSPARRATE   PIC S9(4)V9(2) COMP-3.
             10 PO-DRKI3863-G48SPURSPRVERTNR   PIC X(012).
             10 PO-DRKI3863-G48SPVERTRAGLFZ   PIC S9(4) COMP.
             10 PO-DRKI3863-G48SPVERTRAGSTYP   PIC  X(0002).
             10 PO-DRKI3863-G48SPWKN   PIC  X(0006).
             10 PO-DRKI3863-G48SPZAHLWEISE   PIC X(001).
             10 PO-DRKI3863-G48SPZLASTRUECK   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

