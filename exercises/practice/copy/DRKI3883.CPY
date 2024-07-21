      *AUTHOR.        -#-----------.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO-8333!30.04.21! NEU                             ! WHO
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3883
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3883.
           05 PI-DRKI3883-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3883-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3883-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3883-UFKT-QUIT               VALUE 3.
 
           05 PI-DRKI3883-KEY.
              10 PI-DRKI3883-G48SPPLANNR         PIC  X(15).


       01  PO-DRKI3883.
           05  PO-DRKI3883-KEY.
             10 PO-DRKI3883-G48SPPLANNR   PIC  X(0002).
             10 PO-DRKI3883-G48SPVNR   PIC  X(0015).
             10 PO-DRKI3883-G48SPWAEHRUNGSKZ   PIC  X(0003).
      *
           05  PO-DRKI3883-RC                     PIC  X(003).
              88 PO-DRKI3883-RC-OK              VALUE 'OK '.
              88 PO-DRKI3883-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3883-RC-EOF             VALUE 'EOF'.
      *
 
           05  PO-DRKI3883-DATEN.
             10 PO-DRKI3883-G48SPSPARRATE   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3883-G48SPSCHLIESSDATUM PIC X(008).
             10 PO-DRKI3883-G48SPZAHLWEISE  PIC X(001). 
             10 PO-DRKI3883-G48SPDATVERTKUEND  PIC X(008).              
             10 PO-DRKI3883-G48SPVERTRAGSTYP  PIC  X(0002).
             10 PO-DRKI3883-G48SPDELKZ        PIC X(0001). 
             
      *
      ******* ENDE COPY-MEMBER ***************************************
